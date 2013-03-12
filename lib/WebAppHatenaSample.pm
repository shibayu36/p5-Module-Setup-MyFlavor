
package WebAppHatenaSample;
use strict;
use warnings;
use base 'Module::Setup::Flavor';
1;

=head1

WebAppHatenaSample - pack from webapp-hatena-sample

=head1 SYNOPSIS

  WebAppHatenaSample-setup --init --flavor-class=+WebAppHatenaSample new_flavor

=cut

__DATA__

---
file: Makefile.PL
template: |
  use inc::Module::Install;
  
  name '[% dist %]';
  version '0.01';
  
  requires 'Module::Install::Base'; # Test::Name::FromLine <- Config::Env
  
  # ---- for framework ---
  requires 'Carp';
  requires 'JSON::XS';
  requires 'Class::Accessor::Lite';
  requires 'Class::Accessor::Lite::Lazy';
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
  requires 'Text::Xslate::Bridge::TT2Like';
  requires 'Try::Tiny';
  requires 'URI';
  requires 'URI::QueryParam';
  requires 'FormValidator::Lite';
  
  requires 'DateTime';
  requires 'DateTime::Format::MySQL';
  
  requires 'Plack';
  requires 'Plack::Middleware::ReverseProxy';
  requires 'Plack::Middleware::Scope::Container';
  
  # ---- for DB ----
  requires 'DBIx::Sunny';
  requires 'SQL::NamedPlaceholder';
  requires 'Scope::Container::DBI';
  requires 'SQL::Maker';
  requires 'DBD::mysql';
  
  # ---- for server ----
  requires 'Starlet';
  requires 'Server::Starter';
  
  # ---- for test ----
  test_requires 'Test::More';
  test_requires 'Test::Exception';
  test_requires 'Test::Differences';
  test_requires 'Test::Class';
  test_requires 'Test::WWW::Mechanize::PSGI';
  
  test_requires 'Devel::KYTProf';
  
  WriteAll;
---
file: db/schema.sql
template: |
  CREATE TABLE user (
      `user_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
      `name` VARBINARY(32) NOT NULL,
  
      `created` TIMESTAMP NOT NULL DEFAULT '0000-00-00 00:00:00',
  
      PRIMARY KEY (user_id),
      UNIQUE KEY (name),
      KEY (created)
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
---
dir: inc/.author
---
file: inc/Module/Install.pm
template: |
  #line 1
  package Module::Install;
  
  # For any maintainers:
  # The load order for Module::Install is a bit magic.
  # It goes something like this...
  #
  # IF ( host has Module::Install installed, creating author mode ) {
  #     1. Makefile.PL calls "use inc::Module::Install"
  #     2. $INC{inc/Module/Install.pm} set to installed version of inc::Module::Install
  #     3. The installed version of inc::Module::Install loads
  #     4. inc::Module::Install calls "require Module::Install"
  #     5. The ./inc/ version of Module::Install loads
  # } ELSE {
  #     1. Makefile.PL calls "use inc::Module::Install"
  #     2. $INC{inc/Module/Install.pm} set to ./inc/ version of Module::Install
  #     3. The ./inc/ version of Module::Install loads
  # }
  
  use 5.005;
  use strict 'vars';
  use Cwd        ();
  use File::Find ();
  use File::Path ();
  
  use vars qw{$VERSION $MAIN};
  BEGIN {
  	# All Module::Install core packages now require synchronised versions.
  	# This will be used to ensure we don't accidentally load old or
  	# different versions of modules.
  	# This is not enforced yet, but will be some time in the next few
  	# releases once we can make sure it won't clash with custom
  	# Module::Install extensions.
  	$VERSION = '1.06';
  
  	# Storage for the pseudo-singleton
  	$MAIN    = undef;
  
  	*inc::Module::Install::VERSION = *VERSION;
  	@inc::Module::Install::ISA     = __PACKAGE__;
  
  }
  
  sub import {
  	my $class = shift;
  	my $self  = $class->new(@_);
  	my $who   = $self->_caller;
  
  	#-------------------------------------------------------------
  	# all of the following checks should be included in import(),
  	# to allow "eval 'require Module::Install; 1' to test
  	# installation of Module::Install. (RT #51267)
  	#-------------------------------------------------------------
  
  	# Whether or not inc::Module::Install is actually loaded, the
  	# $INC{inc/Module/Install.pm} is what will still get set as long as
  	# the caller loaded module this in the documented manner.
  	# If not set, the caller may NOT have loaded the bundled version, and thus
  	# they may not have a MI version that works with the Makefile.PL. This would
  	# result in false errors or unexpected behaviour. And we don't want that.
  	my $file = join( '/', 'inc', split /::/, __PACKAGE__ ) . '.pm';
  	unless ( $INC{$file} ) { die <<"END_DIE" }
  
  Please invoke ${\__PACKAGE__} with:
  
  	use inc::${\__PACKAGE__};
  
  not:
  
  	use ${\__PACKAGE__};
  
  END_DIE
  
  	# This reportedly fixes a rare Win32 UTC file time issue, but
  	# as this is a non-cross-platform XS module not in the core,
  	# we shouldn't really depend on it. See RT #24194 for detail.
  	# (Also, this module only supports Perl 5.6 and above).
  	eval "use Win32::UTCFileTime" if $^O eq 'MSWin32' && $] >= 5.006;
  
  	# If the script that is loading Module::Install is from the future,
  	# then make will detect this and cause it to re-run over and over
  	# again. This is bad. Rather than taking action to touch it (which
  	# is unreliable on some platforms and requires write permissions)
  	# for now we should catch this and refuse to run.
  	if ( -f $0 ) {
  		my $s = (stat($0))[9];
  
  		# If the modification time is only slightly in the future,
  		# sleep briefly to remove the problem.
  		my $a = $s - time;
  		if ( $a > 0 and $a < 5 ) { sleep 5 }
  
  		# Too far in the future, throw an error.
  		my $t = time;
  		if ( $s > $t ) { die <<"END_DIE" }
  
  Your installer $0 has a modification time in the future ($s > $t).
  
  This is known to create infinite loops in make.
  
  Please correct this, then run $0 again.
  
  END_DIE
  	}
  
  
  	# Build.PL was formerly supported, but no longer is due to excessive
  	# difficulty in implementing every single feature twice.
  	if ( $0 =~ /Build.PL$/i ) { die <<"END_DIE" }
  
  Module::Install no longer supports Build.PL.
  
  It was impossible to maintain duel backends, and has been deprecated.
  
  Please remove all Build.PL files and only use the Makefile.PL installer.
  
  END_DIE
  
  	#-------------------------------------------------------------
  
  	# To save some more typing in Module::Install installers, every...
  	# use inc::Module::Install
  	# ...also acts as an implicit use strict.
  	$^H |= strict::bits(qw(refs subs vars));
  
  	#-------------------------------------------------------------
  
  	unless ( -f $self->{file} ) {
  		foreach my $key (keys %INC) {
  			delete $INC{$key} if $key =~ /Module\/Install/;
  		}
  
  		local $^W;
  		require "$self->{path}/$self->{dispatch}.pm";
  		File::Path::mkpath("$self->{prefix}/$self->{author}");
  		$self->{admin} = "$self->{name}::$self->{dispatch}"->new( _top => $self );
  		$self->{admin}->init;
  		@_ = ($class, _self => $self);
  		goto &{"$self->{name}::import"};
  	}
  
  	local $^W;
  	*{"${who}::AUTOLOAD"} = $self->autoload;
  	$self->preload;
  
  	# Unregister loader and worker packages so subdirs can use them again
  	delete $INC{'inc/Module/Install.pm'};
  	delete $INC{'Module/Install.pm'};
  
  	# Save to the singleton
  	$MAIN = $self;
  
  	return 1;
  }
  
  sub autoload {
  	my $self = shift;
  	my $who  = $self->_caller;
  	my $cwd  = Cwd::cwd();
  	my $sym  = "${who}::AUTOLOAD";
  	$sym->{$cwd} = sub {
  		my $pwd = Cwd::cwd();
  		if ( my $code = $sym->{$pwd} ) {
  			# Delegate back to parent dirs
  			goto &$code unless $cwd eq $pwd;
  		}
  		unless ($$sym =~ s/([^:]+)$//) {
  			# XXX: it looks like we can't retrieve the missing function
  			# via $$sym (usually $main::AUTOLOAD) in this case.
  			# I'm still wondering if we should slurp Makefile.PL to
  			# get some context or not ...
  			my ($package, $file, $line) = caller;
  			die <<"EOT";
  Unknown function is found at $file line $line.
  Execution of $file aborted due to runtime errors.
  
  If you're a contributor to a project, you may need to install
  some Module::Install extensions from CPAN (or other repository).
  If you're a user of a module, please contact the author.
  EOT
  		}
  		my $method = $1;
  		if ( uc($method) eq $method ) {
  			# Do nothing
  			return;
  		} elsif ( $method =~ /^_/ and $self->can($method) ) {
  			# Dispatch to the root M:I class
  			return $self->$method(@_);
  		}
  
  		# Dispatch to the appropriate plugin
  		unshift @_, ( $self, $1 );
  		goto &{$self->can('call')};
  	};
  }
  
  sub preload {
  	my $self = shift;
  	unless ( $self->{extensions} ) {
  		$self->load_extensions(
  			"$self->{prefix}/$self->{path}", $self
  		);
  	}
  
  	my @exts = @{$self->{extensions}};
  	unless ( @exts ) {
  		@exts = $self->{admin}->load_all_extensions;
  	}
  
  	my %seen;
  	foreach my $obj ( @exts ) {
  		while (my ($method, $glob) = each %{ref($obj) . '::'}) {
  			next unless $obj->can($method);
  			next if $method =~ /^_/;
  			next if $method eq uc($method);
  			$seen{$method}++;
  		}
  	}
  
  	my $who = $self->_caller;
  	foreach my $name ( sort keys %seen ) {
  		local $^W;
  		*{"${who}::$name"} = sub {
  			${"${who}::AUTOLOAD"} = "${who}::$name";
  			goto &{"${who}::AUTOLOAD"};
  		};
  	}
  }
  
  sub new {
  	my ($class, %args) = @_;
  
  	delete $INC{'FindBin.pm'};
  	{
  		# to suppress the redefine warning
  		local $SIG{__WARN__} = sub {};
  		require FindBin;
  	}
  
  	# ignore the prefix on extension modules built from top level.
  	my $base_path = Cwd::abs_path($FindBin::Bin);
  	unless ( Cwd::abs_path(Cwd::cwd()) eq $base_path ) {
  		delete $args{prefix};
  	}
  	return $args{_self} if $args{_self};
  
  	$args{dispatch} ||= 'Admin';
  	$args{prefix}   ||= 'inc';
  	$args{author}   ||= ($^O eq 'VMS' ? '_author' : '.author');
  	$args{bundle}   ||= 'inc/BUNDLES';
  	$args{base}     ||= $base_path;
  	$class =~ s/^\Q$args{prefix}\E:://;
  	$args{name}     ||= $class;
  	$args{version}  ||= $class->VERSION;
  	unless ( $args{path} ) {
  		$args{path}  = $args{name};
  		$args{path}  =~ s!::!/!g;
  	}
  	$args{file}     ||= "$args{base}/$args{prefix}/$args{path}.pm";
  	$args{wrote}      = 0;
  
  	bless( \%args, $class );
  }
  
  sub call {
  	my ($self, $method) = @_;
  	my $obj = $self->load($method) or return;
          splice(@_, 0, 2, $obj);
  	goto &{$obj->can($method)};
  }
  
  sub load {
  	my ($self, $method) = @_;
  
  	$self->load_extensions(
  		"$self->{prefix}/$self->{path}", $self
  	) unless $self->{extensions};
  
  	foreach my $obj (@{$self->{extensions}}) {
  		return $obj if $obj->can($method);
  	}
  
  	my $admin = $self->{admin} or die <<"END_DIE";
  The '$method' method does not exist in the '$self->{prefix}' path!
  Please remove the '$self->{prefix}' directory and run $0 again to load it.
  END_DIE
  
  	my $obj = $admin->load($method, 1);
  	push @{$self->{extensions}}, $obj;
  
  	$obj;
  }
  
  sub load_extensions {
  	my ($self, $path, $top) = @_;
  
  	my $should_reload = 0;
  	unless ( grep { ! ref $_ and lc $_ eq lc $self->{prefix} } @INC ) {
  		unshift @INC, $self->{prefix};
  		$should_reload = 1;
  	}
  
  	foreach my $rv ( $self->find_extensions($path) ) {
  		my ($file, $pkg) = @{$rv};
  		next if $self->{pathnames}{$pkg};
  
  		local $@;
  		my $new = eval { local $^W; require $file; $pkg->can('new') };
  		unless ( $new ) {
  			warn $@ if $@;
  			next;
  		}
  		$self->{pathnames}{$pkg} =
  			$should_reload ? delete $INC{$file} : $INC{$file};
  		push @{$self->{extensions}}, &{$new}($pkg, _top => $top );
  	}
  
  	$self->{extensions} ||= [];
  }
  
  sub find_extensions {
  	my ($self, $path) = @_;
  
  	my @found;
  	File::Find::find( sub {
  		my $file = $File::Find::name;
  		return unless $file =~ m!^\Q$path\E/(.+)\.pm\Z!is;
  		my $subpath = $1;
  		return if lc($subpath) eq lc($self->{dispatch});
  
  		$file = "$self->{path}/$subpath.pm";
  		my $pkg = "$self->{name}::$subpath";
  		$pkg =~ s!/!::!g;
  
  		# If we have a mixed-case package name, assume case has been preserved
  		# correctly.  Otherwise, root through the file to locate the case-preserved
  		# version of the package name.
  		if ( $subpath eq lc($subpath) || $subpath eq uc($subpath) ) {
  			my $content = Module::Install::_read($subpath . '.pm');
  			my $in_pod  = 0;
  			foreach ( split //, $content ) {
  				$in_pod = 1 if /^=\w/;
  				$in_pod = 0 if /^=cut/;
  				next if ($in_pod || /^=cut/);  # skip pod text
  				next if /^\s*#/;               # and comments
  				if ( m/^\s*package\s+($pkg)\s*;/i ) {
  					$pkg = $1;
  					last;
  				}
  			}
  		}
  
  		push @found, [ $file, $pkg ];
  	}, $path ) if -d $path;
  
  	@found;
  }
  
  
  
  
  
  #####################################################################
  # Common Utility Functions
  
  sub _caller {
  	my $depth = 0;
  	my $call  = caller($depth);
  	while ( $call eq __PACKAGE__ ) {
  		$depth++;
  		$call = caller($depth);
  	}
  	return $call;
  }
  
  # Done in evals to avoid confusing Perl::MinimumVersion
  eval( $] >= 5.006 ? <<'END_NEW' : <<'END_OLD' ); die $@ if $@;
  sub _read {
  	local *FH;
  	open( FH, '<', $_[0] ) or die "open($_[0]): $!";
  	my $string = do { local $/; <FH> };
  	close FH or die "close($_[0]): $!";
  	return $string;
  }
  END_NEW
  sub _read {
  	local *FH;
  	open( FH, "< $_[0]"  ) or die "open($_[0]): $!";
  	my $string = do { local $/; <FH> };
  	close FH or die "close($_[0]): $!";
  	return $string;
  }
  END_OLD
  
  sub _readperl {
  	my $string = Module::Install::_read($_[0]);
  	$string =~ s/(?:\015{1,2}\012|\015|\012)/\n/sg;
  	$string =~ s/(\n)\n*__(?:DATA|END)__\b.*\z/$1/s;
  	$string =~ s/\n\n=\w+.+?\n\n=cut\b.+?\n+/\n\n/sg;
  	return $string;
  }
  
  sub _readpod {
  	my $string = Module::Install::_read($_[0]);
  	$string =~ s/(?:\015{1,2}\012|\015|\012)/\n/sg;
  	return $string if $_[0] =~ /\.pod\z/;
  	$string =~ s/(^|\n=cut\b.+?\n+)[^=\s].+?\n(\n=\w+|\z)/$1$2/sg;
  	$string =~ s/\n*=pod\b[^\n]*\n+/\n\n/sg;
  	$string =~ s/\n*=cut\b[^\n]*\n+/\n\n/sg;
  	$string =~ s/^\n+//s;
  	return $string;
  }
  
  # Done in evals to avoid confusing Perl::MinimumVersion
  eval( $] >= 5.006 ? <<'END_NEW' : <<'END_OLD' ); die $@ if $@;
  sub _write {
  	local *FH;
  	open( FH, '>', $_[0] ) or die "open($_[0]): $!";
  	foreach ( 1 .. $#_ ) {
  		print FH $_[$_] or die "print($_[0]): $!";
  	}
  	close FH or die "close($_[0]): $!";
  }
  END_NEW
  sub _write {
  	local *FH;
  	open( FH, "> $_[0]"  ) or die "open($_[0]): $!";
  	foreach ( 1 .. $#_ ) {
  		print FH $_[$_] or die "print($_[0]): $!";
  	}
  	close FH or die "close($_[0]): $!";
  }
  END_OLD
  
  # _version is for processing module versions (eg, 1.03_05) not
  # Perl versions (eg, 5.8.1).
  sub _version ($) {
  	my $s = shift || 0;
  	my $d =()= $s =~ /(\.)/g;
  	if ( $d >= 2 ) {
  		# Normalise multipart versions
  		$s =~ s/(\.)(\d{1,3})/sprintf("$1%03d",$2)/eg;
  	}
  	$s =~ s/^(\d+)\.?//;
  	my $l = $1 || 0;
  	my @v = map {
  		$_ . '0' x (3 - length $_)
  	} $s =~ /(\d{1,3})\D?/g;
  	$l = $l . '.' . join '', @v if @v;
  	return $l + 0;
  }
  
  sub _cmp ($$) {
  	_version($_[1]) <=> _version($_[2]);
  }
  
  # Cloned from Params::Util::_CLASS
  sub _CLASS ($) {
  	(
  		defined $_[0]
  		and
  		! ref $_[0]
  		and
  		$_[0] =~ m/^[^\W\d]\w*(?:::\w+)*\z/s
  	) ? $_[0] : undef;
  }
  
  1;
  
  # Copyright 2008 - 2012 Adam Kennedy.
---
file: lib/____var-module_path-var____.pm
template: |
  package [% module %];
  
  use strict;
  use warnings;
  use utf8;
  
  use Class::Load qw(load_class);
  use Guard;  # guard
  use HTTP::Status ();
  use Try::Tiny;
  
  use [% module %]::Error;
  use [% module %]::Context;
  use [% module %]::Config;
  use [% module %]::Logger qw(CRIT);
  
  sub as_psgi {
      my $class = shift;
      return sub {
          my $env = shift;
          return $class->run($env);
      };
  }
  
  sub run {
      my ($class, $env) = @_;
  
      my $context = [% module %]::Context->from_env($env);
      my $dispatch;
      try {
          my $route = $context->route or [% module %]::Error->throw(404);
          $route->{engine} or [% module %]::Error->throw(404);
          $env->{'[% module.split("::").join(".") FILTER lower %].route'} = $route;
  
          my $engine = join '::', __PACKAGE__, 'Engine', $route->{engine};
          my $action = $route->{action} || 'default';
          $dispatch = "$engine#$action";
  
          load_class $engine;
  
          $class->before_dispatch($context);
  
          my $handler = $engine->can($action) or [% module %]::Error->throw(501);
  
          $engine->$handler($context);
      }
      catch {
          my $e = $_;
          my $res = $context->request->new_response;
          if (eval { $e->isa('[% module %]::Error') }) {
              my $message = $e->{message} || HTTP::Status::status_message($e->{code});
              $res->code($e->{code});
              $res->header('X-Error-Message' => $message);
              $res->content_type('text/plain');
              $res->content($message);
          }
          else {
              CRIT "%s", $e;
              my $message = (config->env =~ /production/) ? 'Internal Server Error' : $e;
              $res->code(500);
              $res->content_type('text/plain');
              $res->header('X-Error-Message' => $message);
              $res->content($message);
          }
          $context->response($res);
      }
      finally {
          $class->after_dispatch($context);
      };
  
      $context->res->headers->header(X_Dispatch => $dispatch);
      return $context->res->finalize;
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
  
  use Config::ENV '[% module.split("::").join("_") FILTER upper %]_ENV', export => 'config';
  use Path::Class qw(file);
  
  my $Router = [% module %]::Config::Route->make_router;
  my $Root = file(__FILE__)->dir->parent->parent->parent->absolute;
  
  sub router { $Router }
  sub root { $Root }
  
  common {
  };
  
  $ENV{SERVER_PORT} ||= 3000;
  config default => {
      origin => "http://local.hatena.ne.jp:$ENV{SERVER_PORT}",
  };
  
  config production => {
  };
  
  config local => {
      parent('default'),
      db => {
          [% module.split("::").join("_") FILTER lower %] => {
              user     => 'nobody',
              password => 'nobody',
              dsn      => 'dbi:mysql:dbname=[% module.split("::").join("_") FILTER lower %];host=localhost',
          },
      },
      db_timezone => 'UTC',
  };
  
  config test => {
      parent('default'),
  
      db => {
          [% module.split("::").join("_") FILTER lower %] => {
              user     => 'nobody',
              password => 'nobody',
              dsn      => 'dbi:mysql:dbname=[% module.split("::").join("_") FILTER lower %]_test;host=localhost',
          },
      },
      db_timezone => 'UTC',
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
  use [% module %]::Config;
  
  use Carp ();
  use Encode ();
  use URI;
  use URI::QueryParam;
  
  use Class::Accessor::Lite::Lazy (
      rw_lazy => [ qw(request response route stash db) ],
      rw      => [ qw(env) ],
      new     => 1,
  );
  
  use [% module %]::Error;
  use [% module %]::DBI::Factory;
  
  ### Properties
  
  sub from_env {
      my ($class, $env) = @_;
      return $class->new(env => $env);
  }
  
  sub _build_request {
      my $self = shift;
  
      return undef unless $self->env;
      return [% module %]::Request->new($self->env);
  };
  
  sub _build_response {
      my $self = shift;
      return $self->request->new_response(200);
  };
  
  sub _build_route {
      my $self = shift;
      return [% module %]::Config->router->match($self->env);
  };
  
  sub _build_stash { +{} };
  
  *req = \&request;
  *res = \&response;
  
  ### HTTP Response
  
  sub render_file {
      my ($self, $file, $args) = @_;
      $args //= {};
  
      require [% module %]::View::Xslate;
      my $content = [% module %]::View::Xslate->render_file($file, {
          c => $self,
          %{ $self->stash },
          %$args
      });
      return $content;
  }
  
  sub html {
      my ($self, $file, $args) = @_;
  
      my $content = $self->render_file($file, $args);
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
  
  sub error {
      my ($self, $code, $message, %opts) = @_;
      [% module %]::Error->throw($code, $message, %opts);
  }
  
  sub uri_for {
      my ($self, $path_query) = @_;
      my $uri = URI->new(config->param('origin'));
      $uri->path_query($path_query);
      return $uri;
  }
  
  ### DB Access
  sub _build_db {
      my ($self) = @_;
      return [% module %]::DBI::Factory->new;
  }
  
  sub dbh {
      my ($self, $name) = @_;
      return $self->db->dbh($name);
  }
  
  1;
---
file: lib/____var-module_path-var____/DBI.pm
template: |
  package [% module %]::DBI;
  
  use strict;
  use warnings;
  use utf8;
  
  use parent 'DBIx::Sunny';
  use SQL::NamedPlaceholder;
  use Carp ();
  
  sub _expand_args (@) {
      my ($query, @args) = @_;
  
      if (@args == 1 && ref $args[0] eq 'HASH') {
          ( $query, my $binds ) = SQL::NamedPlaceholder::bind_named($query, $args[0]);
          @args = @$binds;
      }
  
      return ($query, @args);
  }
  
  package [% module %]::DBI::db;
  
  use strict;
  use warnings;
  use utf8;
  
  use parent -norequire => 'DBIx::Sunny::db';
  use Class::Load qw(load_class);
  
  sub select_one {
      my $self = shift;
      return $self->SUPER::select_one([% module %]::DBI::_expand_args(@_));
  }
  
  sub select_row {
      my $self = shift;
      return $self->SUPER::select_row([% module %]::DBI::_expand_args(@_));
  }
  
  sub select_all {
      my $self = shift;
      return $self->SUPER::select_all([% module %]::DBI::_expand_args(@_));
  }
  
  sub select_row_as {
      my $self = shift;
      my $class = pop;
      my $row = $self->select_row(@_);
      load_class($class);
      return $row && $class->new($row);
  }
  
  sub select_all_as {
      my $self = shift;
      my $class = pop;
      my $rows = $self->select_all(@_);
      load_class($class);
      return [ map { $class->new($_) } @$rows ];
  }
  
  sub query {
      my $self = shift;
      return $self->SUPER::query([% module %]::DBI::_expand_args(@_));
  }
  
  sub prepare {
      my $self = shift;
      my ($query) = @_;
  
      return $self->SUPER::prepare(@_);
  }
  
  sub __set_comment {
      my $self = shift;
      my $query = shift;
  
      my $trace;
      my $i = 0;
      while ( my @caller = caller($i) ) {
          my $file = $caller[1];
          $file =~ s!\*/!*\//!g;
          $trace = "/* $file line $caller[2] */";
          last if $caller[0] ne ref($self) && $caller[0] !~ m<\b(?:DBIx?|DBD)\b> && $file !~ /^\(eval \d+\)$/;
          $i++;
      }
      $query =~ s!^\s+!!;
      $query =~ s!\s*$! $trace!;
      $query =~ s!\s*\n\s*! !g; # 改行も除く…
      $query;
  }
  
  package [% module %]::DBI::st;
  use parent -norequire => 'DBIx::Sunny::st';
  
  1;
---
file: lib/____var-module_path-var____/Error.pm
template: |
  package [% module %]::Error;
  
  use strict;
  use warnings;
  use utf8;
  
  sub throw {
      my ($class, $code, $message, %opts) = @_;
      die $class->new(
          code    => $code,
          message => $message,
          %opts,
      );
  }
  
  sub new {
      my ($class, %opts) = @_;
      bless \%opts, $class;
  }
  
  1;
---
file: lib/____var-module_path-var____/Logger.pm
template: |
  package [% module %]::Logger;
  
  use strict;
  use warnings;
  use utf8;
  
  use Path::Class;
  use JSON::XS;
  use Exporter::Lite;
  
  our @EXPORT_OK = qw(DEBUG INFO WARN CRIT);
  our $HANDLE = \*STDERR;
  our $PRINT = sub {
      my ($time, $level, $mess, $trace) = @_;
      print { $HANDLE } "$time [$level] $mess @ $trace\n";
  };
  
  sub _log {
      my ($level) = @_;
      if ($level eq 'DEBUG' && !$ENV{DEBUG}) {
          sub { };
      } else {
          sub {
              my @caller = caller(0);
              my $time  = do {
                  my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime(time);
                  sprintf(
                      "%04d-%02d-%02d %02d:%02d:%02d",
                      $year + 1900,
                      $mon + 1, $mday, $hour, $min, $sec
                  );
              };
              my $trace = "$caller[1] # $caller[2]";
              my $mess  = sprintf(shift, map { [% module %]::Logger->new($_) } @_);
              $mess =~ s{\s+}{ }g;
              $PRINT->($time, $level, $mess, $trace);
          };
      }
  }
  
  *DEBUG = _log('DEBUG');
  *INFO  = _log('INFO');
  *WARN  = _log('WARN');
  *CRIT  = _log('CRIT');
  
  # copy from Log::Minimal;
  package
      [% module %]::Logger;
  
  use strict;
  use warnings;
  use base qw/Exporter/;
  use Data::Dumper;
  use Scalar::Util qw/blessed/;
  
  use overload
      '""' => \&stringify,
      '0+' => \&numeric,
      fallback => 1;
  
  sub new {
      my ($class, $value) = @_;
      bless \$value, $class;
  }
  
  sub stringify {
      my $self = shift;
      my $value = $$self;
      if ( blessed($value) && (my $stringify = overload::Method( $value, '""' ) || overload::Method( $value, '0+' )) ) {
          $value = $stringify->($value);
      }
      dumper($value);
  }
  
  sub numeric {
      my $self = shift;
      my $value = $$self;
      if ( blessed($value) && (my $numeric = overload::Method( $value, '0+' ) || overload::Method( $value, '""' )) ) {
          $value = $numeric->($value);
      }
      $value;
  }
  
  sub dumper {
      my $value = shift;
      if ( defined $value && ref($value) ) {
          local $Data::Dumper::Terse = 1;
          local $Data::Dumper::Indent = 0; 
          $value = Data::Dumper::Dumper($value);
      }
      $value;
  }
  
  1;
  __END__
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
  use Sub::Name;
  
  use DateTime;
  use DateTime::Format::MySQL;
  
  use [% module %]::Config;
  
  sub datetime_from_db ($) {
      my $dt = DateTime::Format::MySQL->parse_datetime( shift );
      $dt->set_time_zone(config->param('db_timezone'));
      $dt->set_formatter( DateTime::Format::MySQL->new );
      $dt;
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
              action => 'default',
          };
          connect '/user/list' => {
              engine => 'User',
              action => 'list',
          };
          connect '/user/register' => {
              engine => 'User',
              action => 'register',
          } => { method => 'POST' };
      };
  }
  
  1;
---
file: lib/____var-module_path-var____/DBI/Factory.pm
template: |
  package [% module %]::DBI::Factory;
  
  use strict;
  use warnings;
  use utf8;
  
  use [% module %]::Config;
  use Carp ();
  
  use Scope::Container::DBI;
  
  sub new {
      my ($class) = @_;
      return bless +{}, $class;
  }
  
  sub dbconfig {
      my ($self, $name) = @_;
      my $dbconfig = config->param('db') // Carp::croak 'required db setting';
      return $dbconfig->{$name} // Carp::croak qq(db config for '$name' does not exist);
  }
  
  sub dbh {
      my ($self, $name) = @_;
  
      my $db_config = $self->dbconfig($name);
      my $user      = $db_config->{user} or Carp::croak qq(user for '$name' does not exist);
      my $password  = $db_config->{password} or Carp::croak qq(password for '$name' does not exist);
      my $dsn       = $db_config->{dsn} or Carp::croak qq(dsn for '$name' does not exist);
  
      my $dbh = Scope::Container::DBI->connect($dsn, $user, $password, {
          RootClass => '[% module %]::DBI',
      });
      return $dbh;
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
      my ($class, $c) = @_;
      $c->html('index.html');
  }
  
  1;
  __END__
---
file: lib/____var-module_path-var____/Engine/User.pm
template: |
  package [% module %]::Engine::User;
  
  use strict;
  use warnings;
  use utf8;
  
  use FormValidator::Lite;
  
  sub list {
      my ($class, $c) = @_;
  
      my $users = $c->dbh('[% module.split("::").join("_") FILTER lower %]')->select_all_as(q[
          SELECT * FROM user
            ORDER BY created desc
      ], "[% module %]::Model::User");
  
      $c->html('user/list.html', {
          users => $users,
      });
  }
  
  sub register {
      my ($class, $c) = @_;
  
      my $validator = FormValidator::Lite->new($c->req);
      $validator->check(
          name => ['NOT_NULL', [REGEXP => qr/^[a-zA-Z][a-zA-Z0-9_-]{2,31}$/]],
      );
  
      if ($validator->has_error) {
          return $c->redirect('/user/list');
      }
  
      $c->dbh('[% module.split("::").join("_") FILTER lower %]')->query(q[
          INSERT INTO user (name, created)
            VALUES(:name, :created)
      ], {
          name    => $c->req->parameters->{name},
          created => DateTime->now,
      });
  
      $c->redirect('/user/list');
  }
  
  1;
---
file: lib/____var-module_path-var____/Model/User.pm
template: |
  package [% module %]::Model::User;
  
  use strict;
  use warnings;
  use utf8;
  
  use Class::Accessor::Lite (
      ro => [qw(
          user_id
          name
      )],
      new => 1,
  );
  
  use [% module %]::Util;
  
  sub created {
      my ($self) = @_;
      $self->{_created} ||= eval {
          [% module %]::Util::datetime_from_db($self->{created});
      };
  }
  
  1;
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
      cache     => 0,
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
      my ($class, $file, $args) = @_;
      my $content = $tx->render($file, $args);
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
  my $root = config->root;
  
  builder {
      # enable 'ReverseProxy';
      enable 'Runtime';
      enable 'Head';
  
      if (config->env eq 'production') {
          my $access_log = Path::Class::file($ENV{ACCESS_LOG} || "$root/log/access_log");
          my $error_log  = Path::Class::file($ENV{ERROR_LOG}  || "$root/log/error_log");
  
          $_->dir->mkpath for $access_log, $error_log;
  
          my $fh_access = $access_log->open('>>')
              or die "Cannot open >> $access_log: $!";
          my $fh_error  = $error_log->open('>>')
              or die "Cannot open >> $error_log: $!";
  
          $_->autoflush(1) for $fh_access, $fh_error;
  
          enable 'AccessLog::Timed', (
              logger => sub {
                  print $fh_access @_;
              },
              format => join("\t",
                  'time:%t',
                  'host:%h',
                  'domain:%V',
                  'req:%r',
                  'status:%>s',
                  'size:%b',
                  'referer:%{Referer}i',
                  'ua:%{User-Agent}i',
                  'taken:%D',
                  'xgid:%{X-Generated-Id}o',
                  'xdispatch:%{X-Dispatch}o',
                  'xrev:%{X-Revision}o',
              )
          );
  
          enable sub {
              my $app = shift;
              sub {
                  my $env = shift;
                  local $[% module %]::Logger::PRINT = sub {
                      my ($time, $level, $mess, $trace) = @_;
                      my $url = sprintf '%s://%s%s', $env->{'psgi.url_scheme'} || 'http', $env->{HTTP_HOST}, $env->{REQUEST_URI};
                      print { $fh_error } "$time [$level] $mess @ $trace <$env->{REQUEST_METHOD} $url>\n";
                  };
                  return $app->($env);
              };
          };
      }
  
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
  
  $ENV{HATENA_SAMPLE_ENV} = $runner->{env} || 'local'; # -E は HATENA_STAFFURL_ENV に伝播
  $runner->{env} = 'development';
  
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
file: t/engine/index.t
template: |
  package t::[% module %]::Engine::Index;
  
  use strict;
  use warnings;
  use utf8;
  use lib 't/lib';
  
  use Test::[% module %] -mech;
  
  sub _get : Test(3) {
      my $mech = create_mech;
      $mech->get_ok('/');
      $mech->title_is('[% module %]');
      $mech->content_contains('[% dist %]');
  }
  
  1;
---
file: t/lib/Test/____var-module_path-var____.pm
template: |
  package Test::[% module %];
  
  use strict;
  use warnings;
  use utf8;
  
  use Path::Class;
  use lib file(__FILE__)->dir->parent->parent->parent->parent->subdir('lib')->stringify;
  use lib glob file(__FILE__)->dir->parent->parent->parent->parent->subdir('modules/*/lib')->stringify;
  
  BEGIN {
      $ENV{HATENA_SAMPLE_ENV} = 'test';
      $ENV{PLACK_ENV} = 'test';
      $ENV{DBI_REWRITE_DSN} ||= 1;
  }
  
  use DBIx::RewriteDSN -rules => q<
      ^(.*?;mysql_socket=.*)$ $1
      ^.*?:dbname=([^;]+?)(?:_test)?(?:;.*)?$ dbi:mysql:dbname=$1_test;host=localhost
      ^(DBI:Sponge:)$ $1
      ^(.*)$ dsn:unsafe:got=$1
  >;
  
  sub import {
      my $class = shift;
  
      strict->import;
      warnings->import;
      utf8->import;
  
      my ($package, $file) = caller;
      # 先に読み込むべきモジュールを先に記述する
      # -common は暗黙的に読み込まれる
      my @options = (
          -common => qq[
              use Test::More;
              use Test::Exception;
              use Test::Differences;
              use Test::Time;
  
              use parent 'Test::Class';
              END {
                  $package->runtests if \$0 eq "\Q$file\E";
              }
          ],
          -mech => qq[
              use HTTP::Status qw(:constants);
              use Test::[% module %]::Mechanize;
          ],
          -factory => qq[
              use Test::[% module %]::Factory;
          ],
      );
  
      my %specified = map { $_ => 1 } -common, @_;
  
      my $code = '';
      while (my ($option, $fragment) = splice @options, 0, 2) {
          $code .= $fragment if delete $specified{$option};
      }
      die 'Invalid options: ' . join ', ', keys %specified if %specified;
  
      eval "package $package; $code";
      die $@ if $@;
  }
  
  1;
---
file: t/lib/Test/____var-module_path-var____/Mechanize.pm
template: |
  package Test::[% module %]::Mechanize;
  
  use strict;
  use warnings;
  use utf8;
  
  use parent qw(Test::WWW::Mechanize::PSGI);
  
  use Test::More ();
  
  use Exporter::Lite;
  our @EXPORT = qw(create_mech);
  
  use [% module %];
  
  sub create_mech (;%) {
      return __PACKAGE__->new(@_);
  }
  
  sub new {
      my ($class, %opts) = @_;
  
      my $self = $class->SUPER::new(
          app     => [% module %]->as_psgi,
          %opts,
      );
  
      return $self;
  }
  
  1;
---
file: t/model/user.t
template: |
  package t::[% module %]::Model::User;
  
  use strict;
  use warnings;
  use utf8;
  use lib 't/lib';
  
  use Test::[% module %];
  
  use DateTime;
  use DateTime::Format::MySQL;
  
  sub _require : Test(startup => 1) {
      my ($self) = @_;
      require_ok '[% module %]::Model::User';
  }
  
  sub _accessor : Test(3) {
      my $now = DateTime->now;
      my $user = [% module %]::Model::User->new(
          user_id => 1,
          name    => 'user_name',
          created => DateTime::Format::MySQL->format_datetime($now),
      );
      is $user->user_id, 1;
      is $user->name, 'user_name';
      is $user->created->epoch, $now->epoch;
  }
  
  1;
---
file: t/object/config.t
template: |
  package t::[% module %]::Config;
  
  use strict;
  use warnings;
  use utf8;
  use lib 't/lib';
  
  use Test::[% module %];
  
  use [% module %]::Config;
  
  sub _config : Test(1) {
      is(config->param('origin'), "http://local.hatena.ne.jp:3000");
  }
  
  1;
---
file: t/object/dbi-factory.t
template: |
  package t::[% module %]::DBI::Factory;
  use strict;
  use warnings;
  use utf8;
  
  use lib 't/lib';
  
  use Test::[% module %];
  
  use [% module %]::DBI::Factory;
  
  sub _use : Test(1) {
      use_ok '[% module %]::DBI::Factory';
  }
  
  sub _dbconfig : Test(3) {
      my $dbfactory = [% module %]::DBI::Factory->new;
      my $db_config = $dbfactory->dbconfig('hatena_sample');
      is $db_config->{user}, 'nobody';
      is $db_config->{password}, 'nobody';
      is $db_config->{dsn}, 'dbi:mysql:dbname=hatena_sample_test;host=localhost';
  }
  
  sub _dbh : Test(1) {
      my $dbfactory = [% module %]::DBI::Factory->new;
      my $dbh = $dbfactory->dbh('hatena_sample');
      ok $dbh;
  
  }
  
  1;
---
file: t/object/dbi.t
template: |
  package t::[% module %]::DBI;
  
  use strict;
  use warnings;
  use utf8;
  use lib 't/lib';
  
  use Test::[% module %];
  
  sub _use : Test(1) {
      use_ok '[% module %]::DBI';
  }
  
  1;
---
file: t/object/util.t
template: |
  package t::[% module %]::DBI::Factory;
  
  use strict;
  use warnings;
  use utf8;
  use lib 't/lib';
  
  use Test::[% module %];
  
  use [% module %]::Util;
  
  sub _use : Test(1) {
      use_ok '[% module %]::Util';
  }
  
  1;
---
file: templates/_wrapper.tt
template: |
  <!doctype html>
  <html lang=ja data-engine="" data-action="defalt">
    <head>
      <meta charset=utf-8>
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>[% module %]</title>
    </head>
    <body>
      [% "[% content %"_"]" %]
    </body>
  </html>
---
file: templates/index.html
template: |
  [% "[% WRAPPER '_wrapper.tt' %" _ "]" %]
  <div class=row>
    [% dist %]
  </div>
  [% "[% END # WRAPPER %"_"]" %]
---
file: templates/user/list.html
template: |
  [% "[% WRAPPER '_wrapper.tt' %"_"]" %]
  <div class=row>
    <div class="user-register">
      <form action="/user/register" method="POST">
        <input name="name" />
        <input type="submit" />
      </form>
    </div>
    <ul>
      [% "[%- FOR user IN users -%"_"]" %]
        <li>
          [% "[% user.name %"_"]" %]
        </li>
      [% "[%- END -%"_"]" %]
    </ul>
  </div>
  [% "[% END # WRAPPER %"_"]" %]
---
config:
  author: shiba_yu36
  class: WebAppHatenaSample
  email: shibayu36@gmail.com
  plugins:
    - Config::Basic
    - Template
    - Additional


