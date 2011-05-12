
use strict;
use warnings;



use ExtUtils::MakeMaker 6.31;



my %WriteMakefileArgs = (
  'ABSTRACT' => 'Perl IDE backend with Emacs frontend',
  'AUTHOR' => 'Johan Lindstrom <johanl@cpan.org>',
  'BUILD_REQUIRES' => {
    'Class::MethodMaker' => '0',
    'Exception::Class' => '0',
    'File::Spec' => '0',
    'File::Temp' => '0',
    'Test::Differences' => '0',
    'Test::Exception' => '0',
    'Test::More' => '0'
  },
  'CONFIGURE_REQUIRES' => {
    'ExtUtils::MakeMaker' => '6.31'
  },
  'DISTNAME' => 'Devel-PerlySense',
  'EXE_FILES' => [
    'bin/perly_sense'
  ],
  'LICENSE' => 'perl',
  'NAME' => 'Devel::PerlySense',
  'PREREQ_PM' => {
    'App::Ack' => '0',
    'Cache::Cache' => '0',
    'Cache::FileCache' => '0',
    'Carp' => '0',
    'Data::Dumper' => '0',
    'Exporter' => '0',
    'File::Basename' => '0',
    'File::Find' => '0',
    'File::Find::Rule' => '0',
    'File::Path' => '0',
    'File::Slurp' => '0',
    'File::Spec::Functions' => '0',
    'File::chdir' => '0',
    'Getopt::Long' => '0',
    'Graph::Easy' => '0',
    'HTTP::Date' => '0',
    'IO::String' => '0',
    'List::MoreUtils' => '0',
    'List::Util' => '0',
    'Module::Pluggable' => '3.5',
    'POSIX' => '0',
    'PPI' => '1.003',
    'PPI::Document' => '0',
    'PPI::Dumper' => '0',
    'Path::Class' => '0.11',
    'Perl::Critic' => '1.105',
    'Perl::Tidy' => '0',
    'Pod::Text' => '0',
    'Pod::Usage' => '0',
    'Spiffy' => '0',
    'Storable' => '0',
    'Text::Table' => '0',
    'Time::HiRes' => '0',
    'YAML::Tiny' => '0'
  },
  'VERSION' => '0.0190',
  'test' => {
    'TESTS' => 't/*.t t/data/project/with-dir/source/t/*.t t/data/project/with-git/source/bogus/t/*.t t/data/project/with-perlysenseproject/source/bogus/t/*.t'
  }
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.56) } ) {
  my $br = delete $WriteMakefileArgs{BUILD_REQUIRES};
  my $pp = $WriteMakefileArgs{PREREQ_PM};
  for my $mod ( keys %$br ) {
    if ( exists $pp->{$mod} ) {
      $pp->{$mod} = $br->{$mod} if $br->{$mod} > $pp->{$mod};
    }
    else {
      $pp->{$mod} = $br->{$mod};
    }
  }
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);



