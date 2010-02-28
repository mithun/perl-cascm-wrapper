use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'CASCM::Wrapper',
    license             => 'perl',
    dist_author         => 'Mithun Ayachit <mithun@cpan.org>',
    dist_version_from   => 'lib/CASCM/Wrapper.pm',
    requires => {
        'Test::More' => 0,
        'version'    => 0,
'File::HomeDir' => 0.64,
'File::Temp' => 0.16,
    },
    add_to_cleanup      => [ 'CASCM-Wrapper-*' ],
);

$builder->create_build_script();
