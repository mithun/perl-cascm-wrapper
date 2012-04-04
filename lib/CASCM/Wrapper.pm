package CASCM::Wrapper;

#######################
# LOAD MODULES
#######################
use 5.006001;

use strict;
use warnings FATAL => 'all';

use File::Temp qw();
use Carp qw(croak carp);

#######################
# VERSION
#######################
our $VERSION = '0.06';

#######################
# SETTINGS
#######################

# Logger
our $log;

#######################
# MODULE METHODS
#######################

# Constructor
sub new {
    my $class = shift;
    my $options_ref = shift || {};

    my $self = {};
    bless $self, $class;
    return $self->_init($options_ref);
} ## end sub new

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
} ## end sub set_context

# load context
sub load_context {
    my $self = shift;
    my $file = shift
        || ( $self->_err("File required but missing") and return );

    if ( not -f $file ) { $self->_err("File $file does not exist"); return; }

    eval {
        require Config::Tiny;
        Config::Tiny->import();
        return 1;
    } or do {
        $self->_err(
            "Please install Config::Tiny if you'd like to load context files"
        );
        return;
    };

    my $config = Config::Tiny->read($file)
        or do { $self->_err("Error reading $file") and return; };

    my $context = {};
    foreach ( keys %{$config} ) {
        if   ( $_ eq '_' ) { $context->{global} = $config->{$_}; }
        else               { $context->{$_}     = $config->{$_}; }
    }

    return $self->set_context($context);
} ## end sub load_context

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
} ## end sub update_context

# Get context
sub get_context { return shift->{_context}; }

# Get error message
sub errstr { return shift->{_errstr}; }

#######################
# CASCM METHODS
#######################

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
sub hdbgctrl  { return shift->_run( 'hdbgctrl',  @_ ); }
sub hdelss    { return shift->_run( 'hdelss',    @_ ); }
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
sub hucache   { return shift->_run( 'hucache',   @_ ); }
sub hudp      { return shift->_run( 'hudp',      @_ ); }
sub hup       { return shift->_run( 'hup',       @_ ); }
sub husrmgr   { return shift->_run( 'husrmgr',   @_ ); }
sub husrunlk  { return shift->_run( 'husrunlk',  @_ ); }

#######################
# INTERNAL METHODS
#######################

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
    my %default_options = (
        'context_file' => 0,
        'dry_run'      => 0,
        'parse_logs'   => 0,
    );

    # Valid options
    my %valid_options = (
        'context_file' => 1,
        'dry_run'      => 1,
        'parse_logs'   => 1,
    );

    # Read options
    my %options = ( %default_options, %{$options_ref} );
    foreach ( keys %options ) {
        croak "Invalid option $_" unless $valid_options{$_};
    }
    $self->{_options} = \%options;

    # Set context
    if ( $options{'context_file'} ) {
        $self->load_context( $options{'context_file'} )
            or croak "Error Loading Context file : " . $self->errstr();
    }

    # Check if we're parsing logs
    if ( $options{'parse_logs'} ) {
        eval {
            require Log::Any;
            Log::Any->import(qw($log));
            return 1;
        }
            or croak
            "Error loading Log::Any. Please install it if you'd like to parse logs";
    } ## end if ( $options{'parse_logs'...})

    # Done initliazing
    return $self;
} ## end sub _init

# Set error
sub _err {
    my $self = shift;
    my $msg  = shift;
    $self->{_errstr} = $msg;
    return 1;
} ## end sub _err

# Execute command
sub _run {
    my $self = shift;
    my $cmd  = shift;
    my @args = @_;

    # Reset error
    $self->_err(qw());

    # Check for context
    my $run_context = {};
    if ( ref $args[0] eq 'HASH' ) { $run_context = shift @args; }

    # Get options
    my $dry_run   = $self->{_options}->{'dry_run'};
    my $parse_log = $self->{_options}->{'parse_logs'};

    # Get cmd context
    my $global_context = $self->get_context()->{global} || {};
    my $cmd_context    = $self->get_context()->{$cmd}   || {};
    my $context = { %{$global_context}, %{$cmd_context}, %{$run_context} };

    # Check if we're parsing logs
    my $default_log = File::Temp->new()->filename;
    if ($parse_log) {

        # Remove existing 'o' & 'oa' from context
        delete $context->{'o'}  if exists $context->{'o'};
        delete $context->{'oa'} if exists $context->{'oa'};

        # Set default log
        $context->{'o'} = $default_log;
    } ## end if ($parse_log)

    # Build argument string
    my $arg_str = q();
    if (@args) { $arg_str = join( ' ', '-arg', @args ); }

    # Get option string for $cmd
    my $opt_str = $self->_get_option_str( $cmd, $context );

    # Dry run
    if ($dry_run) { return "$cmd $arg_str $opt_str"; }

    # Prepare DI file
    my $DIF = File::Temp->new( UNLINK => 0 );
    my $di_file = $DIF->filename;
    print( $DIF "$arg_str $opt_str" )
        or do { $self->_err("Unable to write to $di_file") and return; };

    # Run command
    my $cmd_str = "$cmd -di \"${di_file}\"";
    my $out     = `$cmd_str 2>&1`;
    my $rc      = $?;

    # Cleanup DI file if command didn't remove it
    if ( -f $di_file ) { unlink $di_file; }

    # Parse log
    _parse_log($default_log) if $parse_log;

    # Return
    return $self->_handle_error( $cmd, $rc, $out );
} ## end sub _run

# Get option string
sub _get_option_str {
    my $self    = shift;
    my $cmd     = shift;
    my $context = shift || {};

    my @cmd_options = _get_cmd_options($cmd);

    my @opt_args = qw();
    foreach my $option (@cmd_options) {
        next unless $context->{$option};
        my $val = $context->{$option};
        if   ( $val eq '1' ) { push @opt_args, "-${option}"; }
        else                 { push @opt_args, "-${option}", $val; }
    } ## end foreach my $option (@cmd_options)

    return join( ' ', @opt_args );
} ## end sub _get_option_str

# Command options
sub _get_cmd_options {
    my $cmd = shift;

#<<< Don't touch this ...

    my $options = {
        'common'    => [qw(v o oa wts)],
        'hft'       => [qw(fo a b fs)],
        'hauthsync' => [qw(b usr pw eh)],
        'husrunlk'  => [qw(b usr pw eh)],
        'hsigset'   => [qw(context purge)],
        'hdelss'    => [qw(b en usr pw eh)],
        'hgetusg'   => [qw(b usr pw pu cu)],
        'hpkgunlk'  => [qw(b en usr pw eh)],
        'hsigget'   => [qw(a purge v t gl)],
        'hppolget'  => [qw(b usr pw f eh gl)],
        'hppolset'  => [qw(b usr pw fc eh f)],
        'hsmtp'     => [qw(m p f s d cc bcc)],
        'himpenv'   => [qw(b f usr pw iug eh)],
        'hchu'      => [qw(b usr pw npw ousr eh)],
        'hdbgctrl'  => [qw(b rm rport usr pw eh)],
        'hcp'       => [qw(b en st usr pw pn at eh)],
        'hformsync' => [qw(b all f d hfd usr pw eh)],
        'hdlp'      => [qw(b en pkgs st pn usr pw eh)],
        'hdv'       => [qw(b en st vp usr pw pn s eh)],
        'haccess'   => [qw(b usr pw en rn ha ug ft eh)],
        'hap'       => [qw(b en st pn c usr pw rej eh)],
        'hdp'       => [qw(b en st usr pw pn pb pd eh)],
        'hsql'      => [qw(b f usr pw eh nh s t eh gl)],
        'hudp'      => [qw(b en st usr pw pn ip ap eh)],
        'hchgtype'  => [qw(b rp usr pw bin txt q ext eh)],
        'hcrrlte'   => [qw(b en usr pw epid epname d eh)],
        'hexpenv'   => [qw(b en f usr pw eac cug eug eh)],
        'hspp'      => [qw(fp tp b en st pn usr pw s eh)],
        'hpp'       => [qw(b en st usr pw pb pm pd pn eh)],
        'hmvpkg'    => [qw(b en st usr pw ph pn ten tst eh)],
        'har'       => [qw(b f m musr mpw usr pw eh er rport)],
        'hrt'       => [qw(b f m musr mpw usr pw eh er rport)],
        'hccmrg'    => [qw(b en st p usr pw mc ma tt tb pn eh)],
        'hri'       => [qw(b en st usr pw vp pn ot ob p de eh)],
        'hrmvpth'   => [qw(b en st vp p usr pw pn ot ob de eh)],
        'hcrtpath'  => [qw(b en st rp usr pw p cipn ot ob eh de)],
        'hrefresh'  => [qw(b pr st nst iv pl ps pv debug nolock)],
        'hlv'       => [qw(b en st vp usr pw vn pn s cd ac ss eh)],
        'hcbl'      => [qw(b en rp usr pw ss add rmr rdp rw eh st)],
        'hcpj'      => [qw(b cpj npj act ina tem usr pw dac cug eh)],
        'hpg'       => [qw(b en pg usr pw st bp cpg dpg app dpp eh)],
        'hexecp'    => [qw(b prg m syn asyn usr pw args ma er rport)],
        'husrmgr'   => [qw(b usr pw dlm ow nn cf du cpw swl ad ae eh)],
        'hmvitm'    => [qw(b en st vp np p usr pw pn ot ob ur uk de eh)],
        'hmvpth'    => [qw(b en st vp np p usr pw pn ot ob ur uk de eh)],
        'hrnpth'    => [qw(b en st vp nn p usr pw pn ot ob ur uk de eh)],
        'hsv'       => [qw(b en vp usr pw st p iu io iv it ib id s eh gl)],
        'hrnitm'    => [qw(b en st vp on nn p usr pw pn ot ob ur uk de eh)],
        'hcropmrg'  => [qw(b en1 en2 st1 st2 p1 p2 usr pw pn mo plo eh vfs)],
        'hlr'       => [qw(b cp rp f usr pw rcep c rm rusr rpw eh er rport)],
        'htakess'   => [qw(b en st abv ss usr pw p po pb vp pg ve ts rs pn eh)],
        'hucache'   => [qw(b en st ss purge vp usr pw cacheagent rusr rpw rport eh er)],
        'hcmpview'  => [qw(b en1 en2 st1 vn1 vn2 vp1 vp2 usr pw uv1 uv2 cidc ciic s eh)],
        'hfatt'     => [qw(b fn fid add rem get at usr pw ft comp cp rm rusr rpw eh er rport)],
        'hup'       => [qw(b en p usr pw npn at pr af apg rpg des nt rf ft afo rfo del cf eh)],
        'hrepedit'  => [qw(b rp ppath tpath rnpath oldname newname usr pw all fo ismv isren eh)],
        'hci'       => [qw(b en st p vp usr pw pn ur uk ro d nd de s op bo if ot ob dvp dcp cp rm rusr rpw eh er rport tr)],
        'hsync'     => [qw(b en st vp cp usr pw eh pn br sy av fv iv pl il iol ps pv ss bo to tb rm rusr rpw er purge excl excls rport ced complete)],
        'hco'       => [qw(b en st vp p up br ro sy cu usr pw vn nvs nvf r replace nt ss s pf po bo to tb ced dvp dcp cp op pn rm rusr rpw eh er rport tr)],
        'hrepmngr'  => [qw(cr b usr pw nc coe mvs noext rext addext addvgrp addugrp addsgrp c fc del dup srn drn ndac ld cp rp r cep rm rusr rpw er ren oldname newname isv upd nc co appext nmvs gext remext remvgrp remugrp remsgrp appc mv srp drp all eh rport)],
    };

#>>>
    my @cmd_options =
        sort { lc $a cmp lc $b }
        ( @{ $options->{common} }, @{ $options->{$cmd} } );
    return @cmd_options;
} ## end sub _get_cmd_options

# Handle error/return
sub _handle_error {
    my $self = shift;
    my $cmd  = shift;
    my $rc   = shift;
    my $out  = shift || qw();

    # Standard cases
    my %error = (
        '1' =>
            "Command syntax for $cmd is incorrect. Please check your context setting",
        '2'  => 'Broker not connected',
        '3'  => "$cmd failed",
        '4'  => 'Unexpected error',
        '5'  => 'Invalid login',
        '6'  => 'Server or database down',
        '7'  => 'Incorrect service pack level',
        '8'  => 'Incompatible server version',
        '9'  => 'Exposed password',
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
    } ## end if ( $cmd eq 'hchu' )
    elsif ( $cmd eq 'hco' ) {
        %error = (
            %error,
            '14' => 'No version was found for the file name or pattern',
        );
    } ## end elsif ( $cmd eq 'hco' )
    elsif ( $cmd eq 'hexecp' ) {
        %error = (
            %error,
            '2' =>
                'Broker not connected OR the invoked program did not return a value of its own',
        );
    } ## end elsif ( $cmd eq 'hexecp' )

    # Get error message
    my $msg;
    if ( $rc == -1 ) {
        $msg = "Failed to execute $cmd";
        $msg .= " : $out" if $out;
        $self->_err($msg);
        return;
    } ## end if ( $rc == -1 )
    elsif ( $rc > 0 ) {
        $rc >>= 8;
        $msg = $error{$rc} || "Uknown error";
        $msg .= " : $out" if $out;
        $self->_err($msg);
        return;
    } ## end elsif ( $rc > 0 )

    # Return true
    return 1;
} ## end sub _handle_error

# Parse Log
sub _parse_log {
    my $logfile = shift;

    if ( not -f $logfile ) {
        $log->error("Logfile $logfile does not exist");
        return 1;
    }

    open( my $L, '<', $logfile )
        or do { $log->error("Unable to read $logfile") and return 1; };
    while (<$L>) {
        my $line = $_;
        chomp $line;
        next unless $line;
        next if $line =~ /^[[:blank:]]$/;

        if    ( $line =~ s/^\s*E0\w{7}:\s*//x ) { $log->error($line); }
        elsif ( $line =~ s/^\s*W0\w{7}:\s*//x ) { $log->warn($line); }
        elsif ( $line =~ s/^\s*I0\w{7}:\s*//x ) { $log->info($line); }
        else                                    { $log->info($line); }
    } ## end while (<$L>)
    close $L;
    unlink($logfile) or $log->warn("Unable to delete $logfile");
    return 1;
} ## end sub _parse_log

#######################
1;

__END__

#######################
# POD SECTION
#######################
=pod

=head1 NAME

CASCM::Wrapper - Run CA-SCM (Harvest) commands

	use CASCM::Wrapper;

	# Initialize
	my $cascm = CASCM::Wrapper->new();

	# Set Context
	$cascm->set_context(
	    {    # Set a global context. This is applied to all commands where required
	       global => { b  => 'harvest',
	                   eh => 'user.dfo',
	       },

	       # Set 'hco' specific context, applied only to hco commands
	       hco => { up => 1,
	                vp => '\repository\myapp\src',
	                pn => 'Checkout Items',
	       },

	       # Similarly for 'hci'
	       hci => { vp => '\repository\myapp\src',
	                pn => 'Checkin Items',
	                de => 'Shiny new feature',
	       },

	       # And 'hcp'
	       hcp => { st => 'development',
	                at => 'userid',
	       },
	    }
	) or die $cascm->errstr;

	# Create Package
	my $pkg = 'new_package';
	$cascm->hcp($pkg) or die $cascm->errstr;

	# Checkout files
	my @files = qw(foo.c bar.c);
	$cascm->hco( { p => $pkg }, @files ) or die $cascm->errstr;

	# Update Context
	$cascm->update_context( { hci => { p => $pkg }, } ) or die $cascm->errstr;

	# Checkin files
	$cascm->hci(@files) or die $cascm->errstr;

=head1 DESCRIPTION

This module is a wrapper around CA-SCM (formerly known as Harvest) commands. It
provides a perl-ish interface to setting the context in which each command is
executed, along with optional loading of context from files as well as parsing
output logs.

=head1 CONTEXT

The context is a I<hash of hashes> which contain the following types of keys:

=over

=item global

This specifies the global context. Any context set here will be applied to
every command that uses it.

	my $global_context = {
	                       global => { b  => 'harvest',
	                                   eh => 'user.dfo',
	                       },
	  };

=item command specific

This provides a command specific context. Context set here will be applied only
to those specific commands.

	my $hco_context = {
	                    hco => { up => 1,
	                             vp => '\repository\myapp\src',
	                             pn => 'Checkout Items',
	                    },
	};

=back

The global and command context keys are synonymous with the command line
options detailed in the CA-SCM Reference Manual. Options that do not require a
value should be set to '1'. i.e. C<{hco =E<gt> {up =E<gt> 1} }> is equivalent
to C<hco -up>. The methods are intelligent enough to apply only the context
keys that are used by a command. For e.g. a global context of C<vp> will not
apply to C<hcp>.

The 'common' options I<i> and I<di> are not applicable and ignored for all
commands. See L</SECURITY>

The following methods are available to manage context

=head2 set_context($context)

Sets the context. Old context is forgotten. The argument provided must be a
hash reference

=head2 update_context($context)

Updates the current context. The argument provided must be a hash reference

=head2 load_context($file)

This loads the context from an 'INI' file. The root parameters defines the
global context. Each sectional parameter defines the command specific context.
Old context is forgotten.

	# Load context file at initialization. This will croak if it fails to read the context file
	my $cascm = CASCM::Wrapper->new( { context_file => $file } );

	# Alternatively
	$cascm->load_context($file) or die $cascm->errstr;

This is a sample context file

	# Sample context file

	# Root parameters. These define the 'global' context
	b  = harvest
	eh = user.dfo

	# Sectional parameters. These define the 'command' context

	[hco]
		up = 1
		vp = \repository\myapp\src

	[hcp]
		st = development

B<NOTE:> This method requires L<Config::Tiny> in order to read the context
file.

=head2 get_context()

Returns a hash reference of current context

	my $context = $cascm->get_context();
	use Data::Dumper;
	print Dumper($context);

=head1 CA-SCM METHODS

Almost every 'h' command that uses a context is supported. The command names
are synonymous with the methods used to invoke them.

Every method accepts two optional arguments. The first is an hash reference
that overrides/appends to the context for that method. This allows setting a
context only for that specific method call. The second is an array of arguments
that is passed on to the 'h' command. Any arguments provided is passed using
the '-arg' option.

	# No parameters. Everything required is already set in the context
	$cascm->hdlp() or die $cascm->errstr;

	# Array of arguments
	$cascm->hci( @files ) or die $cascm->errstr;

	# Override/Append to context
	$cascm->hci( { p => 'new_package' }, @files ) or die $cascm->errstr;

The methods can be called in a 'dry run' mode. Where the method returns the
full command line, without executing anything. This can be useful for
debugging.

	$cascm = CASCM::Wrapper->new( { dry_run => 1 } );
	$cascm->set_context($context);
	$cmd = $cascm->hsync();
	print "Calling hsync() would have executed -> $cmd";

The following CA-SCM commands are available as methods

	hap
	har
	hci
	hco
	hcp
	hdp
	hdv
	hft
	hlr
	hlv
	hpg
	hpp
	hri
	hrt
	hsv
	hup
	hcbl
	hchu
	hcpj
	hdlp
	hspp
	hsql
	hudp
	hfatt
	hsmtp
	hsync
	hccmrg
	hdelss
	hexecp
	hmvitm
	hmvpkg
	hmvpth
	hrnitm
	hrnpth
	haccess
	hcrrlte
	hexpenv
	hgetusg
	himpenv
	hrmvpth
	hsigget
	hsigset
	htakess
	hucache
	husrmgr
	husrunlk
	hchgtype
	hcmpview
	hcropmrg
	hcrtpath
	hdbgctrl
	hpkgunlk
	hppolget
	hppolset
	hrefresh
	hrepedit
	hrepmngr
	hauthsync
	hformsync


=head1 SECURITY

This module uses the I<di> option for executing CA-SCM commands. This prevents
any passwords from being exposed while the command is running. The temporary
I<di> file is deleted irrespective if the outcome of the command.

=head1 LOGGING

Since CA-SCM commands output only to log files, this module allows parsing and
logging of a command's output. L<Log::Any> is required to use this feature,
which in turn allows you to use any (supported) Logging mechanism. When using
this, any 'o' or 'oa' options specified in the context will be ignored. Your
scripts will need to load the appropriate L<Log::Any::Adapter> to capture the
log statements. The CA-SCM log is parsed and the messages are logged either as
'INFO', 'WARN' or 'ERROR'.

	# Using Log4perl

	use CASCM::Wrapper;
	use Log::Log4perl;
	use Log::Any::Adapter;

	Log::Log4perl->init('log4perl.conf');
	Log::Any::Adapter->set('Log4perl');

	# Get logger
	my $log = Log::Log4perl->get_logger();

	# Set parse_logs to true. This will croak if Log:Any is not found.
	my $cascm = CASCM::Wrapper->new( { parse_logs => 1 } );

	# Set Context
	my $context = { ... };
	$cascm->set_context($context);

	# Calling the method automatically will parse the log output into the Log4perl object
	# The output is also logged in the 'CASCM::Wrapper' category.

	$cascm->hco(@files) or die $cascm->errstr;

=head1 ERROR HANDLING

All methods return true on success and C<undef> on failure. The error that most
likely caused the I<last> failure can be obtained by calling the C<errstr>
method.

=head1 DEPENDENCIES

CA-SCM r12 (or higher) client. Harvest 7.1 might work, but has not been tested.

The CA-SCM methods depends on the corresponding commands to be available in the
I<PATH>

At least Perl 5.6.1 is required to run.

Optionally, L<Config::Tiny> is required to read context files

Optionally, L<Log::Any> and L<Log::Any::Adapter> is required to parse CA-SCM
log files

=head1 BUGS AND LIMITATIONS

Please report any bugs or feature requests to C<bug-cascm-wrapper@rt.cpan.org>,
or through the web interface at
L<http://rt.cpan.org/Public/Dist/Display.html?Name=CASCM-Wrapper>

=head1 AUTHOR

Mithun Ayachit C<mithun@cpan.org>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2012, Mithun Ayachit. All rights reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlartistic>.

=cut
