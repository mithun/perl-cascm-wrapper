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
our $VERSION = '0.08';

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

# Parse logs
sub parse_logs {
    my $self = shift;
    if (@_) {
        $self->{_options}->{parse_logs} = shift;
        if ( $self->{_options}->{parse_logs} ) {
            eval {
                require Log::Any;
                Log::Any->import(qw($log));
                return 1;
            }
                or croak
                "Error loading Log::Any. Please install it if you'd like to parse logs";
        } ## end if ( $self->{_options}...)
    } ## end if (@_)
    return $self->{_options}->{parse_logs};
} ## end sub parse_logs

# Dry Run
sub dry_run {
    my $self = shift;
    if (@_) { $self->{_options}->{dry_run} = shift; }
    return $self->{_options}->{dry_run};
}

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
    $self->parse_logs(1) if $options{'parse_logs'};

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
    if (@args) {
        $arg_str = join( ' ', map { "-arg=$_" } @args );
    }

    # Get option string for $cmd
    my $opt_str = $self->_get_option_str( $cmd, $context );

    # Dry run
    if ($dry_run) { return "$cmd $arg_str $opt_str"; }

    # Prepare DI file
    my $DIF = File::Temp->new( UNLINK => 0 );
    my $di_file = $DIF->filename;
    print( $DIF "$arg_str $opt_str" )
        or do { $self->_err("Unable to write to $di_file") and return; };
    close($DIF);

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
        'common'    => [qw(o v oa wts)],
        'haccess'   => [qw(b eh en ft ha pw rn ug usr)],
        'hap'       => [qw(b c eh en pn pw st rej usr)],
        'har'       => [qw(b f m eh er pw mpw usr musr rport)],
        'hauthsync' => [qw(b eh pw usr)],
        'hcbl'      => [qw(b eh en pw rp rw ss st add rdp rmr usr)],
        'hccmrg'    => [qw(b p eh en ma mc pn pw st tb tt usr)],
        'hchgtype'  => [qw(b q eh pw rp bin ext txt usr)],
        'hchu'      => [qw(b eh pw npw usr ousr)],
        'hci'       => [qw(b d p s bo cp de eh en er if nd ob op ot pn pw rm ro st tr uk ur vp dcp dvp rpw usr rusr rport)],
        'hcmpview'  => [qw(b s eh pw en1 en2 st1 usr uv1 uv2 vn1 vn2 vp1 vp2 cidc ciic)],
        'hco'       => [qw(b p r s bo br cp cu eh en er nt op pf pn po pw rm ro ss st sy tb to tr up vn vp ced dcp dvp nvf nvs rpw usr rusr rport replace)],
        'hcp'       => [qw(b at eh en pn pw st usr)],
        'hcpj'      => [qw(b eh pw act cpj cug dac ina npj tem usr)],
        'hcropmrg'  => [qw(b eh mo p1 p2 pn pw en1 en2 plo st1 st2 usr vfs)],
        'hcrrlte'   => [qw(b d eh en pw usr epid epname)],
        'hcrtpath'  => [qw(b p de eh en ob ot pw rp st usr cipn)],
        'hdbgctrl'  => [qw(b eh pw rm usr rport)],
        'hdelss'    => [qw(b eh en pw usr)],
        'hdlp'      => [qw(b eh en pn pw st usr pkgs)],
        'hdp'       => [qw(b eh en pb pd pn pw st adp pdr usr vdr)],
        'hdv'       => [qw(b s eh en pn pw st vp usr)],
        'hexecp'    => [qw(b m er ma pw prg syn usr args asyn rport)],
        'hexpenv'   => [qw(b f eh en pw cug eac eug usr)],
        'hfatt'     => [qw(b at cp eh er fn ft pw rm add fid get rem rpw usr comp rusr rport)],
        'hformsync' => [qw(b d f eh pw all hfd usr)],
        'hft'       => [qw(a b fo fs)],
        'hgetusg'   => [qw(b cu pu pw usr)],
        'himpenv'   => [qw(b f eh pw iug usr)],
        'hlr'       => [qw(b c f cp eh er pw rm rp rpw usr rcep rusr rport)],
        'hlv'       => [qw(b s ac cd eh en pn pw ss st vn vp usr)],
        'hmvitm'    => [qw(b p de eh en np ob ot pn pw st uk ur vp usr)],
        'hmvpkg'    => [qw(b eh en ph pn pw st ten tst usr)],
        'hmvpth'    => [qw(b p de eh en np ob ot pn pw st uk ur vp usr)],
        'hpg'       => [qw(b bp eh en pg pw st app cpg dpg dpp usr)],
        'hpkgunlk'  => [qw(b eh en pw usr)],
        'hpp'       => [qw(b eh en pb pd pm pn pw st adp pdr usr vdr)],
        'hppolget'  => [qw(b f eh gl pw usr)],
        'hppolset'  => [qw(b f eh fc pw usr)],
        'hrefresh'  => [qw(b iv pl pr ps pv st nst debug nolock)],
        'hrepedit'  => [qw(b eh fo pw rp all usr ismv isren ppath tpath rnpath newname oldname)],
        'hrepmngr'  => [qw(b c r co cp cr eh er fc ld mv nc nc pw rm rp all cep coe del drn drp dup isv mvs ren rpw srn srp upd usr appc gext ndac nmvs rext rusr noext rport addext appext remext addsgrp addugrp addvgrp newname oldname remsgrp remugrp remvgrp)],
        'hri'       => [qw(b p de eh en ob ot pn pw st vp usr)],
        'hrmvpth'   => [qw(b p de eh en ob ot pn pw st vp usr)],
        'hrnitm'    => [qw(b p de eh en nn ob on ot pn pw st uk ur vp usr)],
        'hrnpth'    => [qw(b p de eh en nn ob ot pn pw st uk ur vp usr)],
        'hrt'       => [qw(b f m eh er pw mpw usr musr rport)],
        'hsigget'   => [qw(a t v gl purge)],
        'hsigset'   => [qw(purge context)],
        'hsmtp'     => [qw(d f m p s cc bcc)],
        'hspp'      => [qw(b s eh en fp pn pw st tp usr)],
        'hsql'      => [qw(b f s t eh eh gl nh pw usr)],
        'hsv'       => [qw(b p s eh en gl ib id io it iu iv pw st vp usr)],
        'hsync'     => [qw(b av bo br cp eh en er fv il iv pl pn ps pv pw rm ss st sy tb to vp ced iol rpw usr excl rusr excls purge rport complete)],
        'htakess'   => [qw(b p eh en pb pg pn po pw rs ss st ts ve vp abv usr)],
        'hucache'   => [qw(b eh en er pw ss st vp rpw usr rusr purge rport cacheagent)],
        'hudp'      => [qw(b ap eh en ip pn pw st usr)],
        'hup'       => [qw(b p af at cf eh en ft nt pr pw rf afo apg del des npn rfo rpg usr)],
        'husrmgr'   => [qw(b ad ae cf du eh nn ow pw cpw dlm swl usr)],
        'husrunlk'  => [qw(b eh pw usr)],
    };

#>>>
    my @cmd_options = sort { lc $a cmp lc $b }
        ( @{ $options->{common} }, @{ $options->{$cmd} } );
    return @cmd_options;
} ## end sub _get_cmd_options

# Handle error/return
sub _handle_error {
    my $self = shift;
    my $cmd  = shift;
    my $rc   = shift;
    my $out  = shift || '';

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

This module is a wrapper around CA Software Change Manager's (formerly known as
Harvest) commands. It provides a perl-ish interface to setting the context in
which each command is executed, along with optional loading of context from
files as well as parsing output logs.

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

=head1 SEE ALSO

L<CA Software Change
Manager|http://www.ca.com/us/products/detail/CA-Software-Change-Manager.aspx>

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
