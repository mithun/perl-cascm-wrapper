use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile( NAME          => 'CASCM::Wrapper',
               AUTHOR        => 'Mithun Ayachit <mithun@cpan.org>',
               VERSION_FROM  => 'lib/CASCM/Wrapper.pm',
               ABSTRACT_FROM => 'lib/CASCM/Wrapper.pm',
               PL_FILES      => {},
               PREREQ_PM     => {
                              'perl'         => '5.006001',
                              'Test::More'   => 0,
                              'Config::Tiny' => 0,
                              'File::Which'  => 0,
               },
               dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
               clean => { FILES    => 'CASCM-Wrapper-*' },
);
