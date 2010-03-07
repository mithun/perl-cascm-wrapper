package CASCM::Wrapper;

use 5.006001;
use warnings;
use strict;
use Carp;

use Config::Tiny;
use File::Which;

## Version
our $VERSION = '0.01';

####################
## Module Methods ##
####################

# Constructor
sub new {
    my $class = shift;
    my $options_ref = shift || {};

    my $self = {};
    bless $self, $class;
    return $self->_init($options_ref);
}

# Set Context
sub set_context {
    my $self = shift;
    my $context = shift || {};

    if ( ref $context ne 'HASH' ) {
        $self->_err("Context must be a hash reference");
        return;
    }

    $self->{_context} = $context;
    return 1;
}

# load context
sub load_context {
    my $self = shift;
    my $file = shift || ( $self->_err("File required but missing") and return );

    if ( not -f $file ) { $self->_err("File $file does not exist"); return; }

    my $config = Config::Tiny->read($file)
      || ( $self->_err("Error when reading $file : $Config::Tiny::errstr")
           and return );

    my $context = {};
    foreach ( keys %{$config} ) {
        if   ( $_ eq '_' ) { $context->{global} = $config->{$_}; }
        else               { $context->{$_}     = $config->{$_}; }
    }

    return $self->set_context($context);
}

# Update Context
sub update_context {
    my $self = shift;
    my $new = shift || {};

    if ( ref $new ne 'HASH' ) {
        $self->_err("Context must be a hash reference");
        return;
    }

    my $context = $self->get_context();

    foreach my $type ( keys %{$new} ) {
        foreach my $key ( keys %{ $new->{$type} } ) {
            $context->{$type}->{$key} = $new->{$type}->{$key};
        }
    }

    return $self->set_context($context);
}

# Get context
sub get_context { return shift->{_context}; }

# Get error message
sub errstr { return shift->{_errstr}; }

###################
## CASCM Methods ##
###################

sub haccess   { return shift->_run( 'haccess',   @_ ); }
sub hap       { return shift->_run( 'hap',       @_ ); }
sub har       { return shift->_run( 'har',       @_ ); }
sub hauthsync { return shift->_run( 'hauthsync', @_ ); }
sub hcbl      { return shift->_run( 'hcbl',      @_ ); }
sub hccmrg    { return shift->_run( 'hccmrg',    @_ ); }
sub hcrrlte   { return shift->_run( 'hcrrlte',   @_ ); }
sub hchgtype  { return shift->_run( 'hchgtype',  @_ ); }
sub hchu      { return shift->_run( 'hchu',      @_ ); }
sub hci       { return shift->_run( 'hci',       @_ ); }
sub hcmpview  { return shift->_run( 'hcmpview',  @_ ); }
sub hco       { return shift->_run( 'hco',       @_ ); }
sub hcp       { return shift->_run( 'hcp',       @_ ); }
sub hcpj      { return shift->_run( 'hcpj',      @_ ); }
sub hcropmrg  { return shift->_run( 'hcropmrg',  @_ ); }
sub hcrtpath  { return shift->_run( 'hcrtpath',  @_ ); }
sub hdlp      { return shift->_run( 'hdlp',      @_ ); }
sub hdp       { return shift->_run( 'hdp',       @_ ); }
sub hdv       { return shift->_run( 'hdv',       @_ ); }
sub hexecp    { return shift->_run( 'hexecp',    @_ ); }
sub hexpenv   { return shift->_run( 'hexpenv',   @_ ); }
sub hfatt     { return shift->_run( 'hfatt',     @_ ); }
sub hformsync { return shift->_run( 'hformsync', @_ ); }
sub hft       { return shift->_run( 'hft',       @_ ); }
sub hgetusg   { return shift->_run( 'hgetusg',   @_ ); }
sub himpenv   { return shift->_run( 'himpenv',   @_ ); }
sub hlr       { return shift->_run( 'hlr',       @_ ); }
sub hlv       { return shift->_run( 'hlv',       @_ ); }
sub hmvitm    { return shift->_run( 'hmvitm',    @_ ); }
sub hmvpkg    { return shift->_run( 'hmvpkg',    @_ ); }
sub hmvpth    { return shift->_run( 'hmvpth',    @_ ); }
sub hpg       { return shift->_run( 'hpg',       @_ ); }
sub hpkgunlk  { return shift->_run( 'hpkgunlk',  @_ ); }
sub hpp       { return shift->_run( 'hpp',       @_ ); }
sub hppolget  { return shift->_run( 'hppolget',  @_ ); }
sub hppolset  { return shift->_run( 'hppolset',  @_ ); }
sub hrefresh  { return shift->_run( 'hrefresh',  @_ ); }
sub hrepedit  { return shift->_run( 'hrepedit',  @_ ); }
sub hrepmngr  { return shift->_run( 'hrepmngr',  @_ ); }
sub hri       { return shift->_run( 'hri',       @_ ); }
sub hrmvpth   { return shift->_run( 'hrmvpth',   @_ ); }
sub hrnitm    { return shift->_run( 'hrnitm',    @_ ); }
sub hrnpth    { return shift->_run( 'hrnpth',    @_ ); }
sub hrt       { return shift->_run( 'hrt',       @_ ); }
sub hsigget   { return shift->_run( 'hsigget',   @_ ); }
sub hsigset   { return shift->_run( 'hsigset',   @_ ); }
sub hsmtp     { return shift->_run( 'hsmtp',     @_ ); }
sub hspp      { return shift->_run( 'hspp',      @_ ); }
sub hsql      { return shift->_run( 'hsql',      @_ ); }
sub hsv       { return shift->_run( 'hsv',       @_ ); }
sub hsync     { return shift->_run( 'hsync',     @_ ); }
sub htakess   { return shift->_run( 'htakess',   @_ ); }
sub hudp      { return shift->_run( 'hudp',      @_ ); }
sub hup       { return shift->_run( 'hup',       @_ ); }
sub husrmgr   { return shift->_run( 'husrmgr',   @_ ); }
sub husrunlk  { return shift->_run( 'husrunlk',  @_ ); }

######################
## Internal Methods ##
######################

# Object initialization
sub _init {
    my $self        = shift;
    my $options_ref = shift;

    # Basic initliazation
    $self->{_options} = {};
    $self->{_context} = {};
    $self->{_errstr}  = qw();

    # Make sure we have a option hash ref
    if ( ref $options_ref ne 'HASH' ) { croak "Hash reference expected"; }

    # Set default options
    my %default_options = ( 'dry_run' => 0, );

    # Valid options
    my %valid_options = ( 'context_file' => 1, 'dry_run' => 1, );

    # Read options
    my %options = ( %default_options, %{$options_ref} );
    foreach ( keys %options ) {
        croak "Invalid option $_" unless $valid_options{$_};
    }
    $self->{_options} = \%options;

    # Set context
    my $ctx_file;
    $ctx_file = $options{'context_file'} if $options{'context_file'};
    $self->load_context($ctx_file) if $ctx_file;

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

# Find Executable
sub _get_exe {
    my $self = shift;
    my $cmd  = shift;

    my $exe = which($cmd)
      || ( $self->_err("Cannot find $cmd in PATH") and return );

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
    my $dry_run = $self->{_options}->{'dry_run'};

    # Get executable
    my $cmd_exe;
    if ($dry_run) { $cmd_exe = $self->_get_exe($cmd) || $cmd; }
    else          { $cmd_exe = $self->_get_exe($cmd) or return; }

    # Get cmd context
    my $global_context = $self->get_context()->{global} || {};
    my $cmd_context    = $self->get_context()->{$cmd}   || {};
    my $context = { %{$global_context}, %{$cmd_context}, %{$run_context} };

    # Default log name.
    my $default_log = "${cmd}.log";

    # Build argument string
    my $arg_str = "-arg ";
    $arg_str .= "$_ " for @args;

    # Get option string for $cmd
    my $opt_str = $self->_get_option_str( $cmd, $context );

    # Prepare DI file
    my $di_file = "${cmd}.di." . time . ".tmp";
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

    # Cleanup DI file if command didn't remove it
    if ( ( not $dry_run ) and ( -f $di_file ) ) { unlink $di_file; }

    # Parse log
    my $log_ref = _parse_log($default_log);

    # Get success/failure
    my $success = $self->_handle_error( $cmd, $rc );

    # Return
    return $success, $log_ref if wantarray;
    return $success;
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
        'common'    => [qw(v wts)],
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
        '1' => 'Command syntax is incorrect. Please check your context setting',
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
    my @lines;
    open my $L, '<', $logfile || return \@lines;
    @lines = <$L>;
    close $L;
    unlink $logfile;
    return \@lines;
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
