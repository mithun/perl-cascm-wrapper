use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
            module_name       => 'CASCM::Wrapper',
            license           => 'perl',
            dist_author       => 'Mithun Ayachit <mithun@cpan.org>',
            dist_version_from => 'lib/CASCM/Wrapper.pm',
            requires          => {
                          'perl'       => '5.006001',
                          'File::Temp' => '0.16',
                          'Test::More' => 0,
            },
            recommends => { 'Config::Tiny'      => 0,
                            'Log::Any'          => 0,
                            'Log::Any::Adapter' => 0,
            },
            add_to_cleanup => ['CASCM-Wrapper-*'],
            meta_add       => {
                resources => {
                    bugtracker =>
                      'http://rt.cpan.org/NoAuth/Bugs.html?Dist=CASCM-Wrapper',
                    repository => 'http://github.com/mithun/perl-cascm-wrapper',
                },
            },
);

$builder->create_build_script();
