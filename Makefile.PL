
use strict;
use warnings;



use ExtUtils::MakeMaker 6.31;



my %WriteMakefileArgs = (
  'ABSTRACT' => 'Perl IDE backend with Emacs frontend',
  'AUTHOR' => 'Johan Lindstrom <johanl@cpan.org>',
  'BUILD_REQUIRES' => {
    'Class::MethodMaker' => '0',
    'Class::Moose::ExtendsQwList1' => '0',
    'Class::Moose::ExtendsQwList2' => '0',
    'Class::Moose::ExtendsScalar' => '0',
    'Class::Moose::RoleQwList1' => '0',
    'Class::Moose::RoleQwList2' => '0',
    'Class::Moose::RoleScalar' => '0',
    'Class::UseBaseBareList1' => '0',
    'Class::UseBaseBareList2' => '0',
    'Class::UseBaseQw1' => '0',
    'Class::UseBaseQw2' => '0',
    'Class::UseBaseScalar' => '0',
    'Exception::Class' => '0',
    'File::Spec' => '0',
    'File::Temp' => '0',
    'Game::Application' => '0',
    'Game::Controller' => '0',
    'Game::Direction' => '0',
    'Game::Event::Timed' => '0',
    'Game::Lawn' => '0',
    'Game::Location' => '0',
    'Game::Object' => '0',
    'Game::Object::Prize' => '0',
    'Game::Object::Wall' => '0',
    'Game::Object::Worm' => '0',
    'Game::Object::Worm::Bot' => '0',
    'Game::ObjectVisible' => '0',
    'Game::UI' => '0',
    'Game::UI::None' => '0',
    'Moose' => '0',
    'Test::Differences' => '0',
    'Test::Exception' => '0',
    'Test::More' => '0',
    'Win32::Console' => '0',
    'Win32::OLE' => '0',
    'Win32::OLE::Const' => '0',
    'Win32::Sound' => '0',
    'Win32::Word::Writer::Table' => '0',
    'Win32::Word::Writer::TableVisible' => '0'
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
    'Cache::Cache' => '0',
    'Cache::FileCache' => '0',
    'Carp' => '0',
    'Data::Dumper' => '0',
    'Devel::CoverX::Covered' => '0',
    'Devel::CoverX::Covered::Db' => '0',
    'Exporter' => '0',
    'File::Basename' => '0',
    'File::Corresponding' => '0',
    'File::Corresponding::Config::Find' => '0',
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
    'Module::Pluggable' => '0',
    'POSIX' => '0',
    'PPI' => '0',
    'PPI::Document' => '0',
    'PPI::Dumper' => '0',
    'Path::Class' => '0',
    'Perl::Critic' => '0',
    'Pod::Text' => '0',
    'Pod::Usage' => '0',
    'Spiffy' => '0',
    'Storable' => '0',
    'Text::Table' => '0',
    'Time::HiRes' => '0',
    'YAML::Tiny' => '0'
  },
  'VERSION' => '0.0188',
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



