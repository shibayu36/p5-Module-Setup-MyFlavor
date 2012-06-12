
package WebAppPlack;
use strict;
use warnings;
use base 'Module::Setup::Flavor';
1;

=head1

WebAppPlack - pack from webapp-plack

=head1 SYNOPSIS

  WebAppPlack-setup --init --flavor-class=+WebAppPlack new_flavor

=cut

__DATA__

---
file: .gitignore
template: "log\ntmp\n"
---
file: Makefile.PL
template: |
  use inc::Module::Install;
  
  name '[% dist %]';
  version '0.01';
  
  requires 'Module::Install::Base'; # Test::Name::FromLine <- Config::Env
  
  # ---- for framework ---
  requires 'Carp';
  requires 'Class::Accessor::Lite';
  requires 'Class::Load';
  requires 'Config::ENV';
  requires 'Encode';
  requires 'Exporter::Lite';
  requires 'Guard';
  requires 'HTTP::Status';
  requires 'Hash::MultiValue';
  requires 'Path::Class';
  requires 'Router::Simple';
  requires 'Sub::Name';
  requires 'Text::Xslate';
  requires 'URI';
  requires 'URI::QueryParam';
  
  requires 'Plack';
  requires 'Plack::Middleware::ReverseProxy';
  requires 'Plack::Middleware::Scope::Container';
  
  # ---- for server ----
  requires 'Starlet';
  requires 'Server::Starter';
  
  test_requires 'Devel::KYTProf';
  
  WriteAll;
---
file: lib/____var-module_path-var____.pm
template: |
  package [% module %];
  
  use [% module %]::Context;
  
  use Class::Load qw(load_class);
  use Guard;  # guard
  use HTTP::Status ();
  
  sub as_psgi {
      my $class = shift;
      return sub {
          my $env = shift;
          return $class->run($env);
      };
  }
  
  my $ContextClass;
  sub run {
      my ($class, $env) = @_;
  
      my $context = [% module %]::Context->from_env($env);
      my $dispatch;
      eval {
          my $route = $context->route or die 404;
          $route->{engine} or die 404;
          $env->{'[% module.split("::").join(".") FILTER lower %].route'} = $route;
  
          my $engine = join '::', __PACKAGE__, 'Engine', $route->{engine};
          my $action = $route->{action} || 'default';
          $dispatch = "$engine#$action";
  
          load_class $engine;
          my $context_class = $ContextClass->{$engine} ||= do {
              my $pkg = ref $context;
              if (my $roles = $engine->can('ROLES')) {
                  $pkg = join '::', __PACKAGE__, 'Context', '__MIXED__', $route->{engine};
                  $roles = join ' ', map { "[% module %]::Context::Role::$_" } @{ $engine->$roles };
                  eval <<"...";
  package $pkg;
  use parent qw(
      [% module %]::Context
      $roles
  );
  ...
                  die $@ if $@;
              }
              $pkg;
          };
  
          $class->before_dispatch($context);
  
          warn $action;
  
          my $handler = $engine->can($action) or die 501;
  
          bless $context, $context_class;
          my $g = guard { bless $context, '[% module %]::Context' };
          $context->$handler;
      };
      $class->after_dispatch($context);
  
      my $res;
      if (my $e = $@) {
          if (my ($code) = ($e =~ /^(\d\d\d) at/)) {
              $res = $context->request->new_response(
                  $code,
                  [ 'Content-Type' => 'text/plain' ],
                  [ "$code " . HTTP::Status::status_message($code) ]
              );
          } else {
              die $e;
          }
      } else {
          $res = $context->response;
      }
  
      $res->headers->header(X_Dispatch => $dispatch);
  
      return $res->finalize;
  }
  
  sub before_dispatch {
      my ($class, $c) = @_;
      # -------- csrfのための何らかの処理が欲しい -----
      # if ($c->req->method eq 'POST') {
      #     if ($c->user) {
      #         my $rkm = $c->req->parameter(body => 'rkm') or die 400;
      #         my $rkc = $c->req->parameter(body => 'rkc') or die 400;
      #         if ($rkm ne $c->user->rkm || $rkc ne $c->user->rkc) {
      #             die 400;
      #         }
      #     } else {
      #         die 400;
      #     }
      # }
  }
  
  sub after_dispatch {
      my ($class, $c) = @_;
  }
  
  1;
---
file: lib/____var-module_path-var____/Config.pm
template: |
  package [% module %]::Config;
  
  use strict;
  use warnings;
  use utf8;
  
  use [% module %]::Config::Route;
  
  use Config::ENV 'PLACK_ENV', export => 'config';
  use Path::Class qw(file);
  
  my $Router = [% module %]::Config::Route->make_router;
  my $Root = file(__FILE__)->dir[% FOREACH p IN module.split("::") %]->parent[% END %]->parent->absolute;
  
  sub router { $Router }
  sub root { $Root }
  
  common {
  };
  
  $ENV{SERVER_PORT} ||= 3000;
  config default => {
      origin => "http://local.hatena.ne.jp:$ENV{SERVER_PORT}",
  };
  
  config local => {
      parent('default'),
  };
  
  config production => {
  };
  
  1;
---
file: lib/____var-module_path-var____/Context.pm
template: |
  package [% module %]::Context;
  
  use strict;
  use warnings;
  use utf8;
  
  use [% module %]::Request;
  use [% module %]::Util qw(cached_sub);
  use [% module %]::Config;
  
  use Carp ();
  use Encode ();
  use URI;
  use URI::QueryParam;
  
  use Class::Accessor::Lite (
      new => 1,
      rw  => [ 'env' ],
  );
  
  ### Properties
  
  sub from_env {
      my ($class, $env) = @_;
      return $class->new(env => $env);
  }
  
  cached_sub request => sub {
      my $self = shift;
  
      return undef unless $self->env;
      return [% module %]::Request->new($self->env);
  };
  
  cached_sub response => sub {
      my $self = shift;
      return $self->request->new_response(200);
  };
  
  *req = \&request;
  *res = \&response;
  
  cached_sub route => sub {
      my $self = shift;
      return [% module %]::Config->router->match($self->env->{PATH_INFO});
  };
  
  cached_sub stash => sub { +{} };
  
  ### HTTP Response
  
  sub render_file {
      my ($self, $file, %args) = @_;
  
      require [% module %]::View::Xslate;
      my $content = [% module %]::View::Xslate->render_file(
          $file,
          c => $self,
          %{ $self->stash },
          %args
      );
      return $content;
  }
  
  sub html {
      my ($self, $file, %args) = @_;
  
      my $content = $self->render_file($file, %args);
      $self->response->code(200);
      $self->response->content_type('text/html; charset=utf-8');
      $self->response->content(Encode::encode_utf8 $content);
  }
  
  sub json {
      my ($self, $hash) = @_;
  
      require JSON::XS;
      $self->response->code(200);
      $self->response->content_type('application/json; charset=utf-8');
      $self->response->content(JSON::XS::encode_json($hash));
  }
  
  sub plain_text {
      my ($self, @lines) = @_;
      $self->response->code(200);
      $self->response->content_type('text/plain; charset=utf-8');
      $self->response->content(join "\n", @lines);
  }
  
  sub redirect {
      my ($self, $url) = @_;
  
      $self->response->code(302);
      $self->response->header(Location => $url);
  }
  
  sub uri_for {
      my ($self, $path_query) = @_;
      my $uri = URI->new(config->param('origin'));
      $uri->path_query($path_query);
      return $uri;
  }
  
  1;
---
file: lib/____var-module_path-var____/Request.pm
template: |
  package [% module %]::Request;
  
  use strict;
  use warnings;
  use utf8;
  
  use parent 'Plack::Request';
  
  use Hash::MultiValue;
  
  sub parameters {
      my $self = shift;
  
      $self->env->{'plack.request.merged'} ||= do {
          my $query = $self->query_parameters;
          my $body  = $self->body_parameters;
          my $path  = $self->route_parameters;
          Hash::MultiValue->new($path->flatten, $query->flatten, $body->flatten);
      };
  }
  
  sub route_parameters {
      my ($self) = @_;
      return $self->env->{'[% module.split("::").join(".") FILTER lower %].route.parameters'} ||=
          Hash::MultiValue->new(%{ $self->env->{'[% module.split("::").join(".") FILTER lower %].route'} });
  }
  
  sub is_xhr {
      my $self = shift;
      return ( $self->header('X-Requested-With') || '' ) eq 'XMLHttpRequest';
  }
  
  1;
---
file: lib/____var-module_path-var____/Util.pm
template: |
  package [% module %]::Util;
  
  use strict;
  use warnings;
  use utf8;
  
  use Carp ();
  use Exporter::Lite;
  use Sub::Name;
  
  our @EXPORT_OK = qw(
      cached_sub
  );
  
  # cached_sub foo => sub { ... };
  # cached_sub foo => '_foo_cache', sub { .. };
  sub cached_sub {
      my ($name, $cache_key, $builder);
  
      if (@_ == 2) {
          ( $name, $cache_key, $builder ) = ( $_[0], $_[0], $_[1] );
      } elsif (@_ == 3) {
          ( $name, $cache_key, $builder ) = @_;
      } else {
          Carp::croak;
      }
  
      my $pkg = caller;
  
      my $builder_name = "__build__$pkg\::$name";
         $builder_name =~ s/:/_/g;
      $builder = subname $builder_name, $builder;
  
      my $code = subname "$pkg\::$name", sub {
          my $self = shift;
          if (@_) {
              return $self->{$cache_key} = $_[0];
          }
          return $self->{$cache_key} if exists $self->{$cache_key};
          return $self->{$cache_key} = $self->$builder;
      };
  
      no strict 'refs';
      *{"$pkg\::$name"} = $code;
  }
  
  1;
  __END__
---
file: lib/____var-module_path-var____/Config/Route.pm
template: |
  package [% module %]::Config::Route;
  use strict;
  use warnings;
  use utf8;
  
  use Router::Simple::Declare;
  
  sub make_router {
      return router {
          connect '/' => {
              engine => 'Index',
          };
      };
  }
  
  1;
---
file: lib/____var-module_path-var____/Engine/Index.pm
template: |
  package [% module %]::Engine::Index;
  
  use strict;
  use warnings;
  use utf8;
  
  sub default {
      my ($c) = @_;
      $c->html('index.html');
  }
  
  1;
  __END__
---
file: lib/____var-module_path-var____/View/Xslate.pm
template: |
  package [% module %]::View::Xslate;
  
  use strict;
  use warnings;
  use utf8;
  
  use [% module %]::Config;
  
  use Text::Xslate qw(mark_raw html_escape);
  
  our $tx = Text::Xslate->new(
      path      => [ config->root->subdir('templates') ],
      cache     => 1,
      cache_dir => config->root->subdir(qw(tmp xslate)),
      syntax    => 'TTerse',
      module    => [ qw(Text::Xslate::Bridge::TT2Like) ],
      function  => {
          cm => sub { # class method
              my ($class, $method, @args) = @_;
              return $class->$method(@args);
          },
      }
  );
  
  sub render_file {
      my ($class, $file, %args) = @_;
      my $content = $tx->render($file, \%args);
      $content =~ s/^\s+$//mg;
      $content =~ s/>\n+/>\n/g;
      return $content;
  }
  
  1;
---
file: script/app.psgi
template: |
  use strict;
  use warnings;
  use utf8;
  
  use lib 'lib';
  
  use [% module %];
  use [% module %]::Config;
  
  use Path::Class qw(file);
  use Plack::Builder;
  use Plack::Middleware::AccessLog::Timed;
  use Plack::Middleware::Head;
  use Plack::Middleware::Runtime;
  use Plack::Middleware::Scope::Container;
  use Plack::Middleware::Static;
  
  my $app = [% module %]->as_psgi;
  
  builder {
      # enable 'ReverseProxy';
      enable 'Runtime';
      enable 'Head';
  
      enable 'AccessLog::Timed', (
          format => join("\t",
              'time:%t',
              'host:%h',
              'domain:%V',
              'req:%r',
              'status:%>s',
              'size:%b',
              'referer:%{Referer}i',
              'ua:%{User-Agent}i',
              # 'bcookie:%{bcookie}e',
              'taken:%D',
              # 'runtime:%{X-Runtime}o',
              'xgid:%{X-Generated-Id}o',
              'xdispatch:%{X-Dispatch}o',
              'xrev:%{X-Revision}o',
          )
      ) if config->env eq 'production';
  
      enable 'Static', path => qr<^/(?:images|js|css)/>, root => './static/';
      enable 'Static', path => qr<^/favicon\.ico$>,      root => './static/images';
  
      enable 'Scope::Container';
  
      $app;
  };
---
file: script/appup
template: |
  #!/usr/bin/env perl
  use strict;
  use warnings;
  no  warnings 'redefine';
  
  $ENV{DEBUG} = 1 unless defined $ENV{DEBUG};
  $ENV{PERL_LWP_SSL_VERIFY_HOSTNAME} = 0;
  
  my $runner = Plack::Runner::[% module %]->new;
  $runner->parse_options(
      '--port' => 3000,
      '--app'  => 'script/app.psgi',
      '--Reload' => join(',', glob 'lib modules/*/lib'),
      @ARGV,
  );
  
  $ENV{RIDGE_ENV} = $runner->{env}; # -E は RIDGE_ENV に伝播
  $runner->{env}  = 'development';  # PLACK_ENV は development 固定 (このスクリプトは開発時にしか使わないので)
  
  my $options = +{ @{ $runner->{options} } };
  
  # --enable-kyt-prof
  if ($options->{kyt_prof}) {
      require Devel::KYTProf;
      Devel::KYTProf->namespace_regex(qr/^[% module %]/);
  }
  
  # --disable-lessc すると less.pl 立てない
  # unless (exists $options->{lessc} && !$options->{lessc}) {
  #     if (my $pid = fork) {
  #         print STDERR "forking less.pl pid=$pid\n";
  #     } else {
  #         exec $^X, 'script/tools/less.pl';
  #     }
  # }
  
  $runner->run;
  
  package Plack::Runner::[% module %];
  use strict;
  use warnings;
  use parent 'Plack::Runner';
  use Plack::Runner;
  use Plack::Builder;
  
  sub prepare_devel {
      my ($self, $app) = @_;
  
      $app = Plack::Runner::build {
          my $app = shift;
  
          builder {
              enable 'Lint';
              enable 'StackTrace';
  
              enable_if { $_[0]->{PATH_INFO} !~ m<^/(images/|js/|css/|favicon\.ico)> }
                  'AccessLog';
  
              mount '/'   => $app;
          };
      } $app;
  
      push @{$self->{options}}, server_ready => sub {
          my($args) = @_;
          my $name  = $args->{server_software} || ref($args); # $args is $server
          my $host  = $args->{host} || 0;
          my $proto = $args->{proto} || 'http';
          print STDERR "$name: Accepting connections at $proto://$host:$args->{port}/\n";
      };
  
      $app;
  }
---
dir: static/css
---
dir: static/images
---
dir: static/js
---
file: templates/_wrapper.tt
template: |
  <!doctype html>
  <html lang=ja data-engine="[% c.route.engine %]" data-action="[% c.route.action || 'defalt' %]">
    <head>
      <meta charset=utf-8>
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>[% module %]</title>
    </head>
    <body>
  %% content
    </body>
  </html>
---
file: templates/index.html
template: |
  %% WRAPPER '_wrapper.tt'
  <div class=row>
    [% dist %]
  </div>
  %% END  # WRAPPER
---
config:
  author: shiba_yu36
  class: WebAppPlack
  email: shibayu36@gmail.com
  plugins:
    - Config::Basic
    - Template
    - Additional


