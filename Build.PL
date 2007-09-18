use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Devel::PerlySense',
    license             => 'perl',
    dist_author         => 'Johan Lindström <johanl[ÄT]DarSerMan.com>',
    dist_version_from   => 'lib/Devel/PerlySense.pm',
    requires => {
        'App::Ack' => 0,
        'Cache::Cache' => 0,
        'Carp' => 0,
        'Data::Dumper' => 0,
        'File::Basename' => 0,
        'File::Find::Rule' => 0,
        'File::Slurp' => 0,
        'File::Spec' => 0.87,
        'Getopt::Long' => 0,
        'Graph::Easy' => 0.57,
        'List::MoreUtils' => 0,
        'PPI' => 1.003,
        'Path::Class' => 0.11,
        'Pod::Usage' => 0,
        'Spiffy' => 0.20,
        'Test::Differences' => 0,
        'Test::Exception' => 0,
        'Test::More' => 0,
        'Text::Table' => 1.107,
        'YAML::Tiny' => 0,
        'perl'           => '5.6.0',
    },
    script_files => [
        "bin/perly_sense",
       ],  
    add_to_cleanup      => [ 'Devel-PerlySense-*' ],
    create_readme => 1,
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
