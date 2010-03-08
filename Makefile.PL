use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
            NAME          => 'CASCM::Wrapper',
            AUTHOR        => 'Mithun Ayachit <mithun@cpan.org>',
            VERSION_FROM  => 'lib/CASCM/Wrapper.pm',
            ABSTRACT_FROM => 'lib/CASCM/Wrapper.pm',
            PL_FILES      => {},
            PREREQ_PM     => {
                           'perl'       => '5.006001',
                           'Test::More' => 0,
            },
            LICENSE => 'perl',
            dist    => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
            clean    => { FILES => 'CASCM-Wrapper-*' },
            META_ADD => {
                resources => {
                    bugtracker =>
                      'http://rt.cpan.org/NoAuth/Bugs.html?Dist=CASCM-Wrapper',
                    repository => 'http://github.com/mithun/perl-cascm-wrapper',
                },
            },
            MIN_PERL_VERSION => '5.006001',
);
