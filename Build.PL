use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Devel::PerlySense',
    license             => 'perl',
    dist_author         => 'Johan Lindström <johanl[ÄT]DarSerMan.com>',
    dist_version_from   => 'lib\Devel\PerlySense.pm',
    requires => {
        'perl'           => '5.6.0',
        'Test::More' => 0,
        'Spiffy' => 0.20,
        'File::Spec' => 0.87,
        'Path::Class' => 0.11,
        'Data::Dumper' => 0,
        'Carp' => 0,
        'Getopt::Long' => 0,
        'Pod::Usage' => 0,
        'File::Basename' => 0,
        'PPI' => 1.003
    },
    script_files => [
        "bin/perly_sense",
       ],  
    add_to_cleanup      => [ 'Devel-PerlySense-*' ],
    create_readme => 1,
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
