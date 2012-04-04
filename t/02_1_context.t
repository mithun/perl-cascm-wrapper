#!/usr/bin/perl

#####################
# LOAD CORE MODULES
#####################
use strict;
use warnings;
use Test::More;

# Autoflush
$| = 1;

# What are we testing?
my $module = "CASCM::Wrapper";

# Load
use_ok($module) or exit;

# Init
my $cascm = new_ok($module) or exit;

# Test setting a context
my $context = {
    global => {
        b  => 'harvest',
        eh => 'my_creds.dfo',
    },
    hco => {
        vp => '\repo\myapp\src',
        up => 1,
    },
};

# Set context
ok( $cascm->set_context($context) );
is_deeply( $cascm->get_context(), $context );

# Update context
my $updated = {
    global => {
        b  => 'harvest_new',
        eh => 'my_creds.dfo',
    },
    hco => {
        vp => '\repo\myapp\src',
        up => 1,
    },
    hcp => { st => 'dev', },
};
ok(
    $cascm->update_context(
        {
            global => { b  => 'harvest_new' },
            hcp    => { st => 'dev', },
        }
    )
);
is_deeply( $cascm->get_context, $updated );

done_testing();
