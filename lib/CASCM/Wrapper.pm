package CASCM::Wrapper;

use 5.006;
use warnings;
use strict;
use Carp;

use Config::Tiny;
use Cwd qw(abs_path);
use File::HomeDir;
use File::Path::Tiny;
use File::Temp qw(tempfile);
use File::Which;
use Log::Any qw($log);

## Version
our $VERSION = '0.01';

## Module Methods

# Constructor
sub new {
    my $class = shift;
    my $options_ref = shift || {};

    my $self = {};
    bless $self, $class;
    return $self->_init($options_ref);
}

# Set global Context
sub set_global_context {
    my $self = shift;
    my $context = shift || {};

    if ( ref $context ne 'HASH' ) {
        $self->_err("Context must be a hash reference");
        return;
    }

    return $self->_set_context( { global => $context } );
}

# Set command context
sub set_cmd_context {
    my $self = shift;
    my $cmd  = shift
      || ( $self->_err("Command required to set context for it") and return );
    my $context = shift;

    if ( ref $context ne 'HASH' ) {
        $self->_err("Context must be a hash reference");
        return;
    }
    return $self->_set_context( { 'cmd' => { $cmd => $context } } );
}

# Get context
sub get_context {
    my $self = shift;
    return $self->_get_context(@_);
}

# load context
sub load_context {
    my $self = shift;
    my $file = shift || ( $self->_err("File required but missing") and return );

    if ( not -f $file ) { $self->_err("File $file does not exist"); return; }

    my $config = Config::Tiny->read($file)
      || ( $self->_err("Error when reading $file : $Config::Tiny::errstr")
           and return );

    my $context;
    foreach ( keys %{$config} ) {
        my $section;
        if ( $_ eq '_' ) {
            $section = 'global';
            $context->{$section} = $config->{$_};
        }
        else { $section = 'cmd'; $context->{$section}->{$_} = $config->{$_}; }
    }

    return $self->_set_context($context);
}

# Get error message
sub errstr {
    my $self = shift;
    return $self->{_errstr};
}

# Set working directory
sub set_working_directory {
    my $self = shift;
    my $wdir = shift
      || ( $self->_err("No working directory provided") and return );

    # Trim trailing slash
    $wdir =~ s{[\\\/]$}{}x;

    # Make sure it exists and we can write to it
    if ( not -d $wdir ) {
        File::Path::Tiny::mk($wdir) or croak "Unable to create $wdir : $!";
    }
    else { my $fh = tempfile( 'testwdirXXXXX', DIR => $wdir ); }

    $self->{_options}->{'working_directory'} = $wdir;
    return 1;
}

# Get working directory
sub get_working_directory { shift->{_options}->{'working_directory'}; }

## CASCM Methods

## Internal Methods

# Object initialization
sub _init {
    my $self        = shift;
    my $options_ref = shift;

    # Basic initliazation
    $self->{_options}           = {};
    $self->{_context}->{global} = {};
    $self->{_context}->{cmd}    = {};
    $self->{_errstr}            = {};

    # Make sure we have a option hash ref
    if ( ref $options_ref ne 'HASH' ) { croak "Hash reference expected"; }

    # Set default options
    my $user_home = File::HomeDir->my_home();
    my %default_options = ( 'parse_log'         => 0,
                            'working_directory' => "${user_home}/.cascm",
                            'dry_run'           => 0,
    );
    $default_options{'ca_scm_home'} = $ENV{CA_SCM_HOME}
      if ( $ENV{CA_SCM_HOME} and -d $ENV{CA_SCM_HOME} );

    # Default context file
    # Priority for context file
    #   (1) Context file provided at initialization
    #   (2) $ENV{CASCM_CONTEXT}
    #   (3) ~/.cascm/context.ini
    my $default_ctx_file = "${user_home}/.cascm/context.ini";
    $default_options{'context_file'} = $ENV{CASCM_CONTEXT} || $default_ctx_file;
    delete $default_options{'context_file'}
      unless -f $default_options{'context_file'};

    # Valid options
    my %valid_options = ( 'parse_log'         => 1,
                          'ca_scm_home'       => 1,
                          'context_file'      => 1,
                          'working_directory' => 1,
                          'dry_run'           => 1,
    );

    # Read options
    my %options = ( %default_options, %{$options_ref} );
    foreach ( keys %options ) {
        croak "Invalid option $_" unless $valid_options{$_};
    }
    $self->{_options} = \%options;

    # Set context
    my %context;
    my $ctx_file;
    $ctx_file = $options{'context_file'} if $options{'context_file'};
    $self->load_context($ctx_file) if $ctx_file;

    # Set working directory
    $self->set_working_directory( $options{'working_directory'} );

    # Done initliazing
    return $self;
}

# Set error
sub _err {
    my $self = shift;
    my $msg = shift || "Unknown error";
    $self->{_errstr} = $msg;
    return 1;
}

# Set context
sub _set_context {
    my $self = shift;
    my $context = shift || {};
    $self->{_context} = $context;
    return 1;
}

# Get context
sub _get_context {
    my $self = shift;
    my $cmd  = shift;

    my $context = $self->{_context};

    my %cmd_context = ( %{ $context->{global} } );
    if ($cmd) {
        %cmd_context = ( %cmd_context, %{ $context->{cmd}->{$cmd} } )
          if exists $context->{cmd}->{$cmd};
    }

    return %cmd_context if wantarray;
    return \%cmd_context;
}

# Find Executable
sub _get_exe {
    my $self = shift;
    my $cmd  = shift;

    my $exe;
    my $bin = $self->{_options}->{ca_scm_home} || 0;
    if ( $bin and ( -d $bin ) ) {
        if ( $^O ne 'MSWin32' ) { $bin .= "/bin"; }
        $exe = "${bin}/${cmd}";
    }

    $exe ||= which($cmd);
    $exe = abs_path($exe) if $exe;

    return $exe;
}

# Execute command
sub _run {
    my $self = shift;
    my $cmd  = shift;
    my @args = @_;

    # Check for context
    my $run_context = {};
    if ( ref $args[0] eq 'HASH' ) { $run_context = shift @args; }

    # Get options
    my $wdir      = $self->{_options}->{'working_directory'};
    my $parse_log = $self->{_options}->{'parse_log'};
    my $dry_run   = $self->{_options}->{'dry_run'};

    # Get executable
    my $cmd_exe;
    if ($dry_run) { $cmd_exe = $self->_get_exe($cmd) || $cmd; }
    else {
        $cmd_exe = $self->_get_exe($cmd)
          || ( $self->_err("Unable to find executable $cmd") and return );
    }

    # Get cmd context
    my $context = { $self->get_context($cmd), %{$run_context} };

    # Default log name. 'o' gets overwritten if we're parsing logs
    my $default_log = "${wdir}/${cmd}.log";
    if (    ( not exists $context->{'o'} )
         || ( not exists $context->{'oa'} )
         || $parse_log )
    {
        delete $context->{'oa'} if exists $context->{'oa'};
        $context->{'o'} = "\"$default_log\"";
    }

    # Build argument string
    my $arg_str = "-arg ";
    $arg_str .= "\"$_\" " for @args;

    # Get option string for $cmd
    my $opt_str = $self->_get_option_str( $cmd, $context );

    # Prepare DI file
    my $di_file = "${wdir}/${cmd}.di." . time . ".tmp";
    open my $DIF, '>',
      $di_file || ( $self->_err("Unable to create $di_file") and return );
    print $DIF "$arg_str $opt_str"
      || ( $self->_err("Unable to print to $di_file") and return );
    close $DIF;

    # Run command
    my $cmd_str = "$cmd_exe -di=\"${di_file}\"";
    return $cmd_str if $dry_run;    # Dry run
    my @out = `$cmd_str 2>&1`;
    my $rc  = $?;

    # Parse log
    _parse_log($default_log) if $parse_log;

    # Return
    return $self->_handle_error( $cmd, $rc );
}

# Get option string
sub _get_option_str {
    my $self    = shift;
    my $cmd     = shift;
    my $context = shift || {};

    my @cmd_options = _get_cmd_options($cmd);

    my $opt_str;
    foreach my $option (@cmd_options) {
        next unless $context->{$option};
        my $val = $context->{$option};
        if   ( $val eq '1' ) { $opt_str .= "-${option} "; }
        else                 { $opt_str .= "-${option} ${val} "; }
    }

    return $opt_str;
}

# Command options
sub _get_cmd_options {
    my $cmd = shift;

    my $options = {
        'common'    => [qw(o oa v wts)],
        'haccess'   => [qw(b usr pw en rn ha ug ft eh)],
        'hap'       => [qw(b en st pn c usr pw rej eh)],
        'har'       => [qw(b f m musr mpw usr pw eh er)],
        'hauthsync' => [qw(b usr pw eh)],
        'hcbl'      => [qw(b en rp usr pw ss add rmr rdp rw eh st)],
        'hccmrg'    => [qw(b en st p usr pw mc ma tt tb pn eh)],
        'hcrrlte'   => [qw(b en usr pw epid epname d eh)],
        'hchgtype'  => [qw(b rp usr pw bin txt q ext eh)],
        'hchu'      => [qw(b usr pw npw ousr eh)],
        'hci'       => [
            qw(b en st p vp usr pw pn ur uk ro d nd de s op bo if ot ob dvp dcp cp rm rusr rpw eh er)
        ],
        'hcmpview' =>
          [qw(b en1 en2 st1 vn1 vn2 vp1 vp2 usr pw uv1 uv2 cidc ciic s eh)],
        'hco' => [
            qw(b en st vp p up br ro sy cu usr pw vn nvs nvf r replace nt ss s pf po bo to tb ced dvp dcp cp op pn rm rusr rpw eh er)
        ],
        'hcp'      => [qw(b en st usr pw pn at eh)],
        'hcpj'     => [qw(b cpj npj act ina tem usr pw dac cug eh)],
        'hcropmrg' => [qw(b en1 en2 st1 st2 p1 p2 usr pw pn mo plo eh)],
        'hcrtpath' => [qw(b en st rp usr pw p cipn ot ob eh)],
        'hdlp'     => [qw(b en pkgs st pn usr pw eh)],
        'hdp'      => [qw(b en st usr pw pn pb pd eh)],
        'hdv'      => [qw(b en st vp usr pw pn s eh)],
        'hexecp'   => [qw(b prg m syn asyn usr pw args ma er)],
        'hexpenv'  => [qw(b en f usr pw eac cug eug eh)],
        'hfatt' =>
          [qw(b fn fid add rem get at usr pw ft comp cp rm rusr rpw eh er)],
        'hformsync' => [qw(b all f d hfd usr pw eh)],
        'hft'       => [qw(fo a b fs)],
        'hgetusg'   => [qw(b usr pw pu cu)],
        'himpenv'   => [qw(b f usr pw iug eh)],
        'hlr'       => [qw(b cp rp f usr pw rcep c rm rusr rpw eh er)],
        'hlv'       => [qw(b en st vp usr pw vn pn s cd ac ss eh)],
        'hmvitm'    => [qw(b en st vp np p usr pw pn ot ob ur uk de eh)],
        'hmvpkg'    => [qw(b en st usr pw ph pn ten tst eh)],
        'hmvpth'    => [qw(b en st vp np p usr pw pn ot ob ur uk de eh)],
        'hpg'       => [qw(b en pg usr pw st bp cpg dpg app dpp eh)],
        'hpkgunlk'  => [qw(b en usr pw eh)],
        'hpp'       => [qw(b en st usr pw pb pm pd pn eh)],
        'hppolget'  => [qw(b usr pw f eh gl)],
        'hppolset'  => [qw(b usr pw fc eh f)],
        'hrefresh'  => [qw(b pr st nst iv pl ps pv debug nolock)],
        'hrepedit'  => [
            qw(b rp ppath tpath rnpath oldname newname usr pw all fo ismv isren eh)
        ],
        'hrepmngr' => [
            qw(cr b usr pw nc coe mvs noext rext addext addvgrp addugrp addsgrp c fc del dup srn drn ndac ld cp rp r cep rm rusr rpw er ren oldname newname isv upd nc co appext nmvs gext remext remvgrp remugrp remsgrp appc mv srp drp all eh)
        ],
        'hri'     => [qw(b en st usr pw vp pn ot ob p de eh)],
        'hrmvpth' => [qw(b en st vp p usr pw pn ot ob de eh)],
        'hrnitm'  => [qw(b en st vp on nn p usr pw pn ot ob ur uk de eh)],
        'hrnpth'  => [qw(b en st vp nn p usr pw pn ot ob ur uk de eh)],
        'hrt'     => [qw(b f m musr mpw usr pw eh er)],
        'hsigget' => [qw(a purge v t gl)],
        'hsigset' => [qw(context purge)],
        'hsmtp'   => [qw(m p f s d cc bcc)],
        'hspp'    => [qw(fp tp b en st pn usr pw s eh)],
        'hsql'    => [qw(b f usr pw eh nh s t eh gl)],
        'hsv'     => [qw(b en vp usr pw st p iu io iv it ib id s eh gl)],
        'hsync'   => [
            qw(b en st vp cp usr pw eh pn br sy av fv iv pl il iol ps pv ss bo to rm rusr rpw er purge excl excls)
        ],
        'htakess' => [qw(b en st abv ss usr pw p po pb vp pg ve ts rs pn eh)],
        'hudp'    => [qw(b en st usr pw pn ip ap eh)],
        'hup'     => [
            qw(b en p usr pw npn at pr af apg rpg des nt rf ft afo rfo del cf eh)
        ],
        'husrmgr'  => [qw(b usr pw dlm ow nn cf du cpw swl ad ae eh)],
        'husrunlk' => [qw(b usr pw eh)],
    };

    my @cmd_options = ( @{ $options->{common} }, @{ $options->{$cmd} } );
    return @cmd_options;
}

# Handle error/return
sub _handle_error {
    my $self = shift;
    my $cmd  = shift;
    my $rc   = shift;

    # Standard cases
    my %error = (
               '1' => 'Command syntax is incorrect',
               '2' => 'Broker not connected',
               '3' => 'The command line program failed in some anticipated way',
               '4' => 'Unexpected error',
               '5' => 'Invalid login',
               '6' => 'Server or database down',
               '7' => 'Incorrect service pack level',
               '8' => 'Incompatible server version',
               '9' => 'Exposed password',
               '10' => 'Ambiguous arguments',
               '11' => 'Access denied',
               '12' => 'Prelink failed',
               '13' => 'Postlink failed',
    );

    # Special cases
    if ( $cmd eq 'hchu' ) {
        %error = (
            %error,
            '94' =>
              'Password changes executed from the command line using hchu are disabled when external authentication is enabled',
        );
    }
    elsif ( $cmd eq 'hco' ) {
        %error = (
            %error, '14' => 'no version was found for the file name or pattern',
        );
    }

    if ( $rc == -1 ) {
        $self->_err("failed to execute $cmd");
        return;
    }
    elsif ( $rc > 0 ) {
        $rc >>= 8;
        my $msg = $error{$rc} || "Uknown error";
        $self->_err($msg);
        return;
    }

    # Return true
    return 1;
}

# Parse Log
sub _parse_log {
    my $logfile = shift;

    open my $L, '<',
      $logfile || ( $log->error("Unable to read $logfile") and return 1 );
    while (<$L>) {
        chomp;
        my $line = $_;

        if ( $line =~ s/^\s*E\w+:\s*//x ) {
            $log->error($line);
        }
        elsif ( $line =~ s/^\s*W\w+:\s*//x ) {
            $log->warn($line);
        }
        else {
            $line =~ s/^\s*I\w+:\s*//x;
            $log->info($line);
        }
    }
    close $L;
    unlink $logfile || $log->warn("Unable to delete $logfile");
    return 1;
}

#################
1;    # Magic true value required at end of module
__END__

=pod

=head1 NAME

CASCM::Wrapper - [One line description of module's purpose here]

=head1 VERSION

This document describes CASCM::Wrapper version 0.01

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 DEPENDENCIES

=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-cascm-wrapper@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.

=head1 AUTHOR

Mithun Ayachit  C<< <mithun@cpan.org> >>

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2010, Mithun Ayachit C<< <mithun@cpan.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.

=cut
