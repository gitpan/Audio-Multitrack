package Audio::Ecasound::Multitrack;
our $VERSION = '0.95';
use 5.008;
use strict;
no strict qw(subs);
use warnings;
no warnings qw(uninitialized);
no warnings;
use Carp;
use Cwd;
use Tk;
use Storable; 
use Getopt::Std;
use Audio::Ecasound;
use Parse::RecDescent;
use Term::ReadLine;
use Data::YAML::Writer;
use Data::YAML::Reader;
use File::Find::Rule;
use File::Spec::Link;
use IO::All;

## Definitions ##

# 'our' declaration: all packages in the file will see the following
# variables. 

our (
	### 
	$help_screen, 		# 
	@help_topic,    # array of help categories
	%help_topic,    # help text indexed by topic

	$ui, # object providing class behavior for graphic/text functions

	@persistent_vars, # a set of variables we save
					  	# as one big config file
	@effects_static_vars,	# the list of which variables to store and retrieve
	@effects_dynamic_vars,		# same for all chain operators
	@global_vars,    # contained in config file
	@config_vars,    # contained in config file
	@status_vars,    # we will dump them for diagnostic use
	%abbreviations, # for replacements in config files

	$globals,		# yaml assignments for @global_vars
					# for appending to config file
	
	$ecasound_globals, #  Command line switches XX check

	$default,		# the internal default configuration file, as string
					
	$raw_to_disk_format,
	$mix_to_disk_format,
	$mixer_out_format,
	
	$yw,			# yaml writer object
	$yr,			# yaml reader object
	%state_c_ops, 	# intermediate copy for storage/retrieval
	$effects_cache_file, # where we keep info on Ecasound
					# and LADSPA effects, presets, etc.
	
	$ecasound, 		# the name to invoke when we want to kill ecasound

	$grammar, 		# filled by Grammar.pm
	$parser,		# for the objected created by Parse::RecDescent
	%iam_cmd,		# for identifying IAM commands in user input
	@nama_commands,# array of commands my functions provide
	%nama_commands,# as hash as well
	$project_root,	# each project will get a directory here
	                # and one .nama directory, also with 
	
					#
					# $ENV{HOME}/.namarc
					# $ENV{HOME}/nama/paul_brocante
					# $ENV{HOME}/nama/paul_brocante/.wav/vocal_1.wav
					# $ENV{HOME}/nama/paul_brocante/Store.yml
					# $ENV{HOME}/nama/.effects_cache
					# $ENV{HOME}/nama/paul_brocante/.namarc 

					 #this_wav_dir = 
	$state_store_file,	# filename for storing @persistent_vars
	$chain_setup_file, # Ecasound uses this 

	$tk_input_channels,# this many radiobuttons appear
	                # on the menubutton
	%cfg,        # 'config' information as hash
	%devices, 		# alias to data in %cfg
	%opts,          # command line options
	%oid_status,    # state information for the chain templates
	$use_monitor_version_for_mixdown, # sync mixdown version numbers
	              	# to selected track versions , not
					# implemented
	$this_track,	 # the currently active track -- 
					 # used by Text UI only at present
	$this_op,      # currently selected effect # future
	$this_mark,    # current mark  # for future

	@format_fields, # data for replies to text commands

	$project,		# variable for GUI text input
	$project_name,	# current project name
	%state_c,		# for backwards compatilility

	### for effects

	$cop_id, 		# chain operator id, that how we create, 
					# store, find them, adjust them, and destroy them,
					# per track or per project?
	%cops,			 # chain operators stored here
	%copp,			# their parameters for effect update
	@effects,		# static effects information (parameters, hints, etc.)
	%effect_i,		# an index , pn:amp -> effect number
	%effect_j,      # an index , amp -> effect number

	@ladspa_sorted, # ld
	%effects_ladspa,# an index
	$e,				# the name of the variable holding
					# the Ecasound engine object.
					
	%e_bound,		# for displaying hundreds of effects in groups
	$unit,			# jump multiplier, 1 or 60 seconds
	%old_vol,		# a copy of volume settings, for muting
	$length,		# maximum duration of the recording/playback if known
	$jack_on,		# whether we use device jack_alsa, currently unused 
					# signals to replace regular output device with
					# jack_alsa

	@input_chains,	# list of input chain segments 
	@output_chains, # list of output chain segments

	%subst,			# alias, substitutions for the config file
	$tkeca_effects_data,	# original tcl code, actually

	### Widgets
	
	$mw, 			# main window
	$ew, 			# effects window
	$canvas, 		# to lay out the effects window

	# each part of the main window gets its own frame
	# to control the layout better

	$load_frame,
	$add_frame,
	$group_frame,
	$time_frame,
	$clock_frame,
	$oid_frame,
	$track_frame,
	$effect_frame,
	$iam_frame,
	$perl_eval_frame,
	$transport_frame,
	$mark_frame,
	$fast_frame, # forward, rewind, etc.

	## collected widgets (i may need to destroy them)

	$group_rw, # 
	$group_version, # 
	%track_widget, # for chains (tracks)
	%effects_widget, # for effects
	@widget_o, # for templates (oids) 
	%widget_o, # 
	%mark_widget, # marks

	@global_version_buttons, # to set the same version for
						  	#	all tracks
	%marks, 		# the actual times
	$markers_armed, # set true to enable removing a mark
	$mark_remove,   # a button that sets $markers_armed
	$time_step,     # widget shows jump multiplier unit (seconds or minutes)
	$clock, 		# displays clock
	$setup_length,  # displays setup running time

	$project_label,	# project name
	$group_label,	# bus name

	$sn_label,		# project load/save/quit	
	$sn_text,
	$sn_load,
	$sn_new,
	$sn_quit,

	### A separate box for entering IAM (and other) commands
	$iam_label,
	$iam_text,
	$iam, # variable for text entry
	$iam_execute,
	$iam_error, # unused

	# add track gui
	#
	$build_track_label,
	$build_track_text,
	$build_track_add,
	$build_track_rec_label,
	$build_track_rec_text,
	$build_track_mon_label,
	$build_track_mon_text,

	$build_new_take,

	# transport controls
	
	$transport_label,
	$transport_setup_and_connect,
	$transport_setup, # unused
	$transport_connect, # unused
	$transport_disconnect,
	$transport_new,
	$transport_start,
	$transport_stop,

	$old_bg, # initial background color.


	$loopa,  # loopback nodes 
	$loopb,  

	@oids,	# output templates, are applied to the
			# chains collected previously
			# the results are grouped as
			# input, output and intermediate sections

	%inputs,
	%outputs,
	%post_input,
	%pre_output,

	$ladspa_sample_rate,	# used as LADSPA effect parameter fixed at 44100

	$track_name,	# received from Tk text input form
	%track_names,   # belongs in Track.pm
	$ch_r,			# this too, recording channel assignment
	$ch_m,			# monitoring channel assignment, unused


	%L,	# for effects
	%M,
	$debug,				# debug level flags for diagnostics
	$debug2,			# for subroutine names as execute
	$debug3,			# deprecated
						
	$OUT,				# filehandle for Text mode print
	#$commands,	# ref created from commands.yml
	%commands,	# created from commands.yml
	$commands_yml, # the string form of commands.yml

	$save_id, # text variable
	$sn_save_text,# text entry widget
	$sn_save,	# button to save settings
	$sn_recall,	# button to recall settings
	$sn_dump,  # button to dump status

	# new object core
	
	$tracker_bus, 
	$tracker, # tracker_group
	$master_bus, 
	$master, # master_group
	$master_track,
	$mixdown_bus,
	$mixdown,  # group
	$mixdown_track,

	@ti, # track by index (alias @Audio::Ecasound::Multitrack::Track::by_index)
	%tn, # track by name  (alias %Audio::Ecasound::Multitrack::Track::by_name)

	@tracks_data, # staging for saving
	@groups_data, # 
	@marks_data, # 

	$mixer_out_device, # where to send stereo output
	$record_device,    # where to get our inputs


	# rules
	
	$mixer_out,
	$mix_down,
	$mix_link,
	$mix_setup,
	$mix_setup_mon,
	$mon_setup,
	$rec_file,
	$rec_setup,
	$multi,

   # marks and playback looping
   
	$clock_id,		# used in GUI for the Tk event system
					# ->cancel method not reliable
					# for 'repeat' events, so converted to
					# 'after' events
	%event_id,    # events will store themselves with a unique key they provide
	# $event_id{loop} = $loop_event, # Tk events for
	$new_event,   # the Tk widget
	$this_mark,    # current mark
	@loop_endpoints, # they define the loop
	$loop_enable, # whether we automatically loop

   $previous_text_command, # i want to know if i'm repeating
);
 


@global_vars = qw(
						$effects_cache_file
						$ladspa_sample_rate
						$state_store_file
						$chain_setup_file
						$tk_input_channels
						$use_monitor_version_for_mixdown 
						$unit								);
						
@config_vars = qw(
						%abbreviations
						%devices
						$ecasound_globals
						$mix_to_disk_format
						$raw_to_disk_format
						$mixer_out_format
						$mixer_out_device
						$project_root 	
						$record_device			);
						
						

@persistent_vars = qw(

						%cops 			
						$cop_id 		
						%copp 			
						%marks			
						$unit			
						%oid_status		
						%old_vol		
						$this_op
						$jack_on 
						@tracks_data
						@groups_data
						@marks_data
						$loop_enable
						@loop_endpoints
						$length

						);
						# $this_track
					 
@effects_static_vars = qw(

						@effects		
						%effect_i	
						%e_bound
						@ladspa_sorted
						%effects_ladspa		 );


@effects_dynamic_vars = qw(

						%state_c_ops
						%cops    
						$cop_id     
						%copp   
						@marks 	
						$unit				);



@status_vars = qw(

						%state_c
						%state_t
						%copp
						%cops
						%post_input
						%pre_output   
						%inputs
						%outputs      );





# instances needed for yaml_out and yaml_in

$yw = Data::YAML::Writer->new; 
$yr = Data::YAML::Reader->new;

$debug2 = 0; # subroutine names
$debug = 0; # debug statements

## The names of two helper loopback devices:

$loopa = 'loop,111';
$loopb = 'loop,222';


# other initializations
$unit = 1;
$effects_cache_file = '.effects_cache';
$state_store_file = 'State';
$chain_setup_file = 'Setup.ecs'; # For loading by Ecasound
$tk_input_channels = 10;
$use_monitor_version_for_mixdown = 1; # not implemented yet
$ladspa_sample_rate = 44100; # temporary setting
$jack_on = 0; # you should configure jack as device directly in .namarc
$project_root = join_path( $ENV{HOME}, "nama");

## Load my modules

use Audio::Ecasound::Multitrack::Assign qw(:all);
use Audio::Ecasound::Multitrack::Iam;    
use Audio::Ecasound::Multitrack::Tkeca_effects; 
use Audio::Ecasound::Multitrack::Track;
use Audio::Ecasound::Multitrack::Bus;    
use Audio::Ecasound::Multitrack::Mark;

# aliases for concise access

*tn = \%Audio::Ecasound::Multitrack::Track::by_name;
*ti = \@Audio::Ecasound::Multitrack::Track::by_index;

# $ti[3]->rw

# print remove_spaces("bulwinkle is a...");

## Class and Object definitions for package 'Audio::Ecasound::Multitrack'

our @ISA; # no anscestors
use Audio::Ecasound::Multitrack::Object qw(mode);

## The following methods belong to the root class

sub hello {"superclass hello"}

sub new { my $class = shift; return bless {@_}, $class }

use Carp;

sub mainloop { 
	prepare(); 
	$ui->loop;
}
sub status_vars {
	serialize -class => 'Audio::Ecasound::Multitrack', -vars => \@status_vars;
}
sub config_vars {
	serialize -class => 'Audio::Ecasound::Multitrack', -vars => \@config_vars;
}

sub discard_object {
	shift @_ if (ref $_[0]) =~ /Multitrack/;  # HARDCODED
	@_;
}

sub first_run {
	if ( ! -e $project_root ) {

# check for missing components

	my $missing;
		my @a = `which analyseplugin`;
		@a or warn ( <<WARN
LADSPA helper program 'analyseplugin' not found
in $ENV{PATH}, your shell's list of executable 
directories. You will probably have more fun with the LADSPA
libraries and executables installed. http://ladspa.org
WARN
) and  sleep 2 and $missing++;
		my @b = `which ecasound`;
		@b or warn ( <<WARN
Ecasound executable program 'ecasound' not found
in $ENV{PATH}, your shell's list of executable 
directories. This suite depends on the Ecasound
libraries and executables for all audio processing! 
WARN
) and  sleep 2 and $missing++;

my @c = `which file`;
		@c or warn ( <<WARN
BSD utility program 'file' not found
in $ENV{PATH}, your shell's list of executable 
directories. This program is currently required
to be able to play back mixes in stereo.
WARN
) and sleep 2;
if ( $missing ) {
print "You lack $missing main parts of this suite.  
Do you want to continue? [N] ";
		$missing and 
		my $reply = <STDIN>;
		chomp $reply;
		print ("Goodbye.\n"), exit unless $reply =~ /y/i;
}
print <<HELLO;

Aloha. Welcome to Nama and Ecasound. 

Nama places all sound and control files under the
project root directory, which by default is $project_root.

The project root can be specified using the -d command line option, 
and in the configuration file .namarc . 

Would you like to create project root directory $project_root ? [Y] 
HELLO
		my $reply = <STDIN>;
		$reply = lc $reply;
		if ($reply !~ /n/i) {
			create_dir( $project_root);
			print "\n... Done!\n\n";
		} 
	}

		my $config = join_path($ENV{HOME}, ".namarc");
	if ( ! -e $config) {
		print "Configuration file $config not found.\n";
		print "Would you like to create it? [Y] ";
		my $reply = <STDIN>;
		chomp $reply;
		if ($reply !~ /n/i){
			$default =~ s/project_root.*$/project_root: $ENV{HOME}\/nama/m;
			$default > io( $config );
			print "\n.... Done!\n\nPlease edit $config and restart Nama.\n";
		}
		exit;
	}
}
	

	
sub prepare {  

	$debug2 and print "&prepare\n";
	local $debug = 0;
	

	$ecasound  = $ENV{ECASOUND} ? $ENV{ECASOUND} : q(ecasound);
	$e = Audio::Ecasound->new();
	#new_engine();

	### Option Processing ###
	# push @ARGV, qw( -e  );
	#push @ARGV, qw(-d /media/sessions test-abc  );
	getopts('amcegsdtf:', \%opts); 
	#print join $/, (%opts);
	# a: save and reload ALSA state using alsactl
	# d: project root dir
	# c: create project
	# f: configuration file
	# g: gui mode 
	# t: text mode (default)
	# m: don't load state info on initial startup
	# e: don't load static effects data
	# s: don't load static effects data cache
	$project_name = shift @ARGV;
	$debug and print "project name: $project_name\n";

	$debug and print ("\%opts\n======\n", yaml_out(\%opts)); ; 


	read_config();  # from .namarc if we have one

	$project_root = $opts{d} if $opts{d}; # priority to command line option

	$project_root or $project_root = join_path($ENV{HOME}, "nama" );

	first_run();
	
	# init our buses
	
	$tracker_bus  = Audio::Ecasound::Multitrack::Bus->new(
		name => 'Tracker_Bus',
		groups => [qw(Tracker)],
		tracks => [],
		rules  => [ qw( mix_setup rec_setup mon_setup multi rec_file) ],
	);

	# print join (" ", map{ $_->name} Audio::Ecasound::Multitrack::Rule::all_rules() ), $/;

	$master_bus  = Audio::Ecasound::Multitrack::Bus->new(
		name => 'Master_Bus',
		rules  => [ qw(mixer_out mix_link) ],
		groups => ['Master'],
	);
	$mixdown_bus  = Audio::Ecasound::Multitrack::Bus->new(
		name => 'Mixdown_Bus',
		groups => [qw(Mixdown) ],
		rules  => [ qw(mon_setup mix_setup_mon  mix_file ) ],
	);


	prepare_static_effects_data() unless $opts{e};

	#print "keys effect_i: ", join " ", keys %effect_i;
	#map{ print "i: $_, code: $effect_i{$_}->{code}\n" } keys %effect_i;
	#die "no keys";	
	
	# UI object for interface polymorphism
	
	$ui = $opts{t} ? Audio::Ecasound::Multitrack::Text->new 
				   : Audio::Ecasound::Multitrack::Graphical->new ;

	# default to graphic mode with events

	# Tk main window
 	$mw = MainWindow->new;  
	$new_event = $mw->Label();

	$ui->init_gui;
	$ui->transport_gui;
	$ui->time_gui;

	print "project_name: $project_name\n";
	load_project( name => $project_name, create => $opts{c}) 
	  if $project_name;

	# if there is no project name, we still init using pwd

	$debug and print "project_root: ", project_root(), $/;
	$debug and print "this_wav_dir: ", this_wav_dir(), $/;
	$debug and print "project_dir: ", project_dir() , $/;
	1;	
}




sub eval_iam {
	local $debug = 0;	
	$debug2 and print "&eval_iam\n";
	my $command = shift;
	$debug and print "iam command: $command\n";
	my $result = $e->eci($command);
	$debug and print "$result\n" unless $command =~ /register/;
	my $errmsg = $e->errmsg();
	# $errmsg and carp("IAM WARN: ",$errmsg), 
	# not needed ecasound prints error on STDOUT
	$e->errmsg('');
	$result;
}
## configuration file

sub project_root { File::Spec::Link->resolve_all( $project_root ); }

sub config_file { $opts{f} ? $opts{f} : ".namarc" }
sub this_wav_dir {
	$project_name and
	File::Spec::Link->resolve_all(
		join_path( project_root(), $project_name, q(.wav) )  
	);
}
sub project_dir  {$project_name and join_path( project_root(), $project_name)
}

sub global_config{
print ("reading config file $opts{f}\n"), return io( $opts{f})->all if $opts{f} and -r $opts{f};
my @search_path = (project_dir(), $ENV{HOME}, project_root() );
my $c = 0;
	map{ 
#print $/,++$c,$/;
			if (-d $_) {
				my $config = join_path($_, config_file());
				#print "config: $config\n";
				if( -f $config ){ 
					my $yml = io($config)->all ;
					return $yml;
				}
			}
		} ( @search_path) 
}

sub read_config {
	$debug2 and print "&read_config\n";
	
	my $config = shift;
	#print "config: $config";;
	my $yml = length $config > 100 ? $config : $default;
	#print "yml1: $yml";
	strip_all( $yml );
	#print "yml2: $yml";
	if ($yml !~ /^---/){
		$yml =~ s/^\n+//s;
		$yml =~ s/\n+$//s;
		$yml = join "\n", "---", $yml, "...";
	}
#	print "yml3: $yml";
	eval ('$yr->read($yml)') or croak( "Can't read YAML code: $@");
	%cfg = %{  $yr->read($yml)  };
	#print yaml_out( $cfg{abbreviations}); exit;
	*subst = \%{ $cfg{abbreviations} }; # alias
#	*devices = \%{ $cfg{devices} }; # alias
	#print yaml_out( \%subst ); exit;
	walk_tree(\%cfg);
	walk_tree(\%cfg); # second pass completes substitutions
	assign_var( \%cfg, @config_vars); 
	#print "config file: $yml";

}
sub walk_tree {
	#$debug2 and print "&walk_tree\n";
	my $ref = shift;
	map { substitute($ref, $_) } 
		grep {$_ ne q(abbreviations)} 
			keys %{ $ref };
}
sub substitute{
	my ($parent, $key)  = @_;
	my $val = $parent->{$key};
	#$debug and print qq(key: $key val: $val\n);
	ref $val and walk_tree($val)
		or map{$parent->{$key} =~ s/$_/$subst{$_}/} keys %subst;
}
## project handling

sub load_project {
	local $debug = 0;
	#carp "load project: I'm being called from somewhere!\n";
	my %h = @_;
	$debug2 and print "&load_project\n";
	$debug and print yaml_out \%h;
	# return unless $h{name} or $project;

	# we could be called from Tk with variable $project _or_
	# called with a hash with 'name' and 'create' fields.
	
	my $project = remove_spaces($project); # internal spaces to underscores
	$project_name = $h{name} if $h{name};
	$project_name = $project if $project;
	$debug and print "project name: $project_name create: $h{create}\n";
	$project_name and $h{create} and 
		print ("Creating directories....\n"),
		map{create_dir($_)} &project_dir, &this_wav_dir ;
	read_config( global_config() ); 
	initialize_rules();
	initialize_project_data();
	remove_small_wavs(); 
	print "reached here!!!\n";

	retrieve_state( $h{settings} ? $h{settings} : $state_store_file) unless $opts{m} ;
	$opts{m} = 0; # enable 
	
	dig_ruins() unless $#Audio::Ecasound::Multitrack::Track::by_index > 2;


	# possible null if Text mode
	
	$ui->global_version_buttons(); 
	$ui->refresh_group;
	generate_setup() and connect_transport();

#The mix track will always be track index 1 i.e. $ti[$n]
# for $n = 1, And take index 1.
 1;

}

sub initialize_rules {

	package Audio::Ecasound::Multitrack::Rule;
		$n = 0;
		@by_index = ();	# return ref to Track by numeric key
		%by_name = ();	# return ref to Track by name
		%rule_names = (); 
	package Audio::Ecasound::Multitrack;

	$mixer_out = Audio::Ecasound::Multitrack::Rule->new( #  this is the master output
		name			=> 'mixer_out', 
		chain_id		=> 'MixerOut', 

		target			=> 'MON',

	# condition =>	sub{ defined $inputs{mixed}  
	# 	or $debug and print("no customers for mixed, skipping\n"), 0},

		input_type 		=> 'mixed', # bus name
		input_object	=> $loopb, 

		output_type		=> 'device',
		output_object	=> $mixer_out_device,

		status			=> 1,

	);

	$mix_down = Audio::Ecasound::Multitrack::Rule->new(

		name			=> 'mix_file', 
		chain_id		=> 'MixDown',
		target			=> 'REC', 
		
		# sub{ defined $outputs{mixed} or $debug 
		#		and print("no customers for mixed, skipping mixdown\n"), 0}, 

		input_type 		=> 'mixed', # bus name
		input_object	=> $loopb,

		output_type		=> 'file',


		# - a hackish conditional way to include the mixdown format
		# - seems to work
		# - it would be better to add another output type

		output_object   => sub {
			my $track = shift; 
			join " ", $track->full_path, $mix_to_disk_format},

		status			=> 1,
	);

	$mix_link = Audio::Ecasound::Multitrack::Rule->new(

		name			=>  'mix_link',
		chain_id		=>  sub{ my $track = shift; $track->n },
		target			=>  'all',
		condition =>	sub{ defined $inputs{mixed}->{$loopb} },
		input_type		=>  'mixed',
		input_object	=>  $loopa,
		output_type		=>  'mixed',
		output_object	=>  $loopb,
		status			=>  1,
		
	);

	$mix_setup = Audio::Ecasound::Multitrack::Rule->new(

		name			=>  'mix_setup',
		chain_id		=>  sub { my $track = shift; "J". $track->n },
		target			=>  'all',
		input_type		=>  'cooked',
		input_object	=>  sub { my $track = shift; "loop," .  $track->n },
		output_object	=>  $loopa,
		output_type		=>  'cooked',
		condition 		=>  sub{ defined $inputs{mixed}->{$loopb} },
		status			=>  1,
		
	);

	$mix_setup_mon = Audio::Ecasound::Multitrack::Rule->new(

		name			=>  'mix_setup_mon',
		chain_id		=>  sub { my $track = shift; "K". $track->n },
		target			=>  'MON',
		input_type		=>  'cooked',
		input_object	=>  sub { my $track = shift; "loop," .  $track->n },
		output_object	=>  $loopa,
		output_type		=>  'cooked',
		# condition 		=>  sub{ defined $inputs{mixed} },
		condition        => 1,
		status			=>  1,
		
	);



	$mon_setup = Audio::Ecasound::Multitrack::Rule->new(
		
		name			=>  'mon_setup', 
		target			=>  'MON',
		chain_id 		=>	sub{ my $track = shift; $track->n },
		input_type		=>  'file',
		input_object	=>  sub{ my $track = shift; $track->full_path },
		output_type		=>  'cooked',
		output_object	=>  sub{ my $track = shift; "loop," .  $track->n },
		post_input		=>	sub{ my $track = shift; $track->mono_to_stereo},
		condition 		=> 1,
		status			=>  1,
	);
		
	$rec_file = Audio::Ecasound::Multitrack::Rule->new(

		name		=>  'rec_file', 
		target		=>  'REC',
		chain_id	=>  sub{ my $track = shift; 'R'. $track->n },   
		input_type	=>  'device',
		input_object=>  $record_device,
		output_type	=>  'file',
		output_object   => sub {
			my $track = shift; 
			join " ", $track->full_path, $raw_to_disk_format},
		status		=>  1,
	);

	# Rec_setup: must come last in oids list, convert REC
	# inputs to stereo and output to loop device which will
	# have Vol, Pan and other effects prior to various monitoring
	# outputs and/or to the mixdown file output.
			
    $rec_setup = Audio::Ecasound::Multitrack::Rule->new(

		name			=>	'rec_setup', 
		chain_id		=>  sub{ my $track = shift; $track->n },   
		target			=>	'REC',
		input_type		=>  'device',
		input_object	=>  $record_device,
		output_type		=>  'cooked',
		output_object	=>  sub{ my $track = shift; "loop," .  $track->n },
		post_input			=>	sub{ my $track = shift;
										$track->rec_route .
										$track->mono_to_stereo 
										},
		condition 		=> sub { my $track = shift; 
								return "satisfied" if defined
								$inputs{cooked}->{"loop," . $track->n}; 
								0 } ,
		status			=>  1,
	);

	# route cooked signals to multichannel device in the 
	# case that monitor_channel is specified
	#
	# thus we could apply guitar effects for output
	# to a PA mixing board
	#
	# seems ready... just need to turn on status!
	
	$multi  = Audio::Ecasound::Multitrack::Rule->new(  

		name			=>  'multi', 
		target			=>  'REC',
		chain_id 		=>	sub{ my $track = shift; "M".$track->n },
		input_type		=>  'device', # raw
		input_object	=>  sub{ my $track = shift; "loop," .  $track->n},
		output_type		=>  'device',
		output_object	=>  'multi',
		pre_output		=>	sub{ my $track = shift; $track->pre_multi},
		condition 		=> sub { my $track = shift; 
								return "satisfied" if $track->ch_m; } ,
		status			=>  0,
	);


}

sub eliminate_loops {
	# given track
	my $n = shift;
	my $loop_id = "loop,$n";
	return unless defined $inputs{cooked}->{$loop_id} 
		and scalar @{$inputs{cooked}->{$loop_id}} == 1;
	# get customer's id from cooked list and remove it from the list

	my $cooked_id = pop @{ $inputs{cooked}->{$loop_id} }; 

	# i.e. J3

	# add chain $n to the list of the customer's (rule's) output device 
	
	#my $rule  = grep{ $cooked_id =~ /$_->chain_id/ } Audio::Ecasound::Multitrack::Rule::all_rules();  
	my $rule = $mix_setup; 
	defined $outputs{cooked}->{$rule->output_object} 
	  or $outputs{cooked}->{$rule->output_object} = [];
	push @{ $outputs{cooked}->{$rule->output_object} }, $n;


	# remove chain $n as source for the loop

	delete $outputs{cooked}->{$loop_id}; 
	
	# remove customers that use loop as input

	delete $inputs{cooked}->{$loop_id}; 

	# remove cooked customer from his output device list
	# print "customers of output device ",
	#	$rule->output_object, join " ", @{
	#		$outputs{cooked}->{$rule->output_object} };
	#
	@{ $outputs{cooked}->{$rule->output_object} } = 
		grep{$_ ne $cooked_id} @{ $outputs{cooked}->{$rule->output_object} };

	#print $/,"customers of output device ",
	#	$rule->output_object, join " ", @{
	#		$outputs{cooked}->{$rule->output_object} };
	#		print $/;

	# transfer any intermediate processing to numeric chain,
	# deleting the source.
	$post_input{$n} .= $post_input{$cooked_id};
	$pre_output{$n} .= $pre_output{$cooked_id}; 
	delete $post_input{$cooked_id};
	delete $pre_output{$cooked_id};

	# remove loopb when only one customer for  $inputs{mixed}{loop,222}
	
	
	my $ref = ref $inputs{mixed}{$loopb};
	#print "ref: $ref\n";

	if (    $ref =~ /ARRAY/ and 
			(scalar @{$inputs{mixed}{$loopb}} == 1) ){

		$debug and print "i have a loop to eliminate \n";

		# The output device we assume will be chains MixerOut or
		# MixDown

		$ref = ref  $outputs{device}{$mixer_out_device} ;

		 if ( $ref =~ /ARRAY/ ){
	#	 	print "found array\n";
			map{ s/MixerOut/1/ } @{ $outputs{device}{$mixer_out_device} };
		}
		delete $outputs{mixed}{$loopb};
		delete $inputs{mixed}{$loopb};

		$ref = ref  $outputs{file};
		if ( $ref =~ /HASH/ ){

			my @keys = 	keys %{ $outputs{file} } ;
			map{ $ref = ref $outputs{file}{$_};
				  $ref =~ /ARRAY/
					and scalar @{ $outputs{file}{$_}  }
					and map{s/MixDown/1/  } @{ $outputs{file}{$_} }
					} @keys;
		}

	}
	
}

sub initialize_project_data {
	$debug2 and print "&initialize_project_data\n";

	return if transport_running();
	$ui->destroy_widgets();
	$ui->project_label_configure(
		-text => uc $project_name, 
		-background => 'lightyellow',
		); 

	# assign_var($project_init_file, @project_vars);

	%cops        = ();   
	$cop_id           = "A"; # autoincrement
	%copp           = ();    # chain operator parameters, dynamic
	                        # indexed by {$id}->[$param_no]
							# and others
	%old_vol = ();

	@input_chains = ();
	@output_chains = ();

	%track_widget = ();
	%effects_widget = ();
	

	# time related
	
	$markers_armed = 0;
	%marks = ();

	# new Marks
	# print "original marks\n";
	#print join $/, map{ $_->time} Audio::Ecasound::Multitrack::Mark::all();
 	map{ $_->remove} Audio::Ecasound::Multitrack::Mark::all();
	@marks_data = ();
	#print "remaining marks\n";
	#print join $/, map{ $_->time} Audio::Ecasound::Multitrack::Mark::all();
	# volume settings
	
	%old_vol = ();

	# $is_armed = 0;
	
	$Audio::Ecasound::Multitrack::Group::n = 0; 
	@Audio::Ecasound::Multitrack::Group::by_index = ();
	%Audio::Ecasound::Multitrack::Group::by_name = ();

	$Audio::Ecasound::Multitrack::Track::n = 0; 	# incrementing numeric key
	@Audio::Ecasound::Multitrack::Track::by_index = ();	# return ref to Track by numeric key
	%Audio::Ecasound::Multitrack::Track::by_name = ();	# return ref to Track by name
	%Audio::Ecasound::Multitrack::Track::track_names = (); 

	$master = Audio::Ecasound::Multitrack::Group->new(name => 'Master');
	$mixdown =  Audio::Ecasound::Multitrack::Group->new(name => 'Mixdown');
	$tracker = Audio::Ecasound::Multitrack::Group->new(name => 'Tracker', rw => 'REC');

	#print yaml_out( \%Audio::Ecasound::Multitrack::Track::track_names );


# create magic tracks, we will create their GUI later, after retrieve

	$master_track = Audio::Ecasound::Multitrack::SimpleTrack->new( 
		group => 'Master', 
		name => 'Master',
		rw => 'MON',); # no dir, we won't record tracks


	$mixdown_track = Audio::Ecasound::Multitrack::Track->new( 
		group => 'Mixdown', 
		name => 'Mixdown', 
		rw => 'MON'); 

}
## track and wav file handling

sub add_track {

	@_ = discard_object(@_);
	$debug2 and print "&add_track\n";
	return if transport_running();
	my $name = shift;
	$debug and print "name: $name, ch_r: $ch_r, ch_m: $ch_m\n";
	my $track = Audio::Ecasound::Multitrack::Track->new(
		name => $name,
		ch_r => $ch_r,
		ch_m => $ch_m,
	);
	$this_track = $track;
	return if ! $track; 
	$debug and print "ref new track: ", ref $track; 

	# $ch_r and $ch_m are public variables set by GUI
	# Okay, so we will do that for the grammar, too
	# $Audio::Ecasound::Multitrack::chr = 
	
	my $group = $Audio::Ecasound::Multitrack::Group::by_name{$track->group};
	$group->set(rw => 'REC');
	$track_name = $ch_m = $ch_r = undef;

	$ui->track_gui($track->n);
	$debug and print "Added new track!\n", $track->dump;
}

sub dig_ruins { 
	

	# only if there are no tracks , 
	
	$debug2 and print "&dig_ruins";
	return if $tracker->tracks;
	$debug and print "looking for WAV files\n";

	# look for wave files
		
		my $d = this_wav_dir();
		opendir WAV, $d or carp "couldn't open $d: $!";

		# remove version numbers
		
		my @wavs = grep{s/(_\d+)?\.wav//i} readdir WAV;

		my %wavs;
		
		map{ $wavs{$_}++ } @wavs;
		@wavs = keys %wavs;

		$debug and print "tracks found: @wavs\n";
	 
		create_master_and_mix_tracks();

		map{add_track($_)}@wavs;

#	}
}

sub remove_small_wavs {

	# 44 byte stubs left by a recording chainsetup that is 
	# connected by not started

	local $debug = 0;
	$debug2 and print "&remove_small_wavs\n";
	

	$debug and print "this wav dir: ", this_wav_dir(), $/;
         my @wavs = File::Find::Rule ->name( qr/\.wav$/i )
                                        ->file()
                                        ->size(44)
                                        ->extras( { follow => 1} )
                                     ->in( this_wav_dir() );
    $debug and print join $/, @wavs;

	map { unlink $_ } @wavs; 
}

sub add_volume_control {
	my $n = shift;
	
	my $vol_id = cop_add({
				chain => $n, 
				type => 'ea',
				cop_id => $ti[$n]->vol, # often undefined
				});
	
	$ti[$n]->set(vol => $vol_id);  # save the id for next time
	$vol_id;
}
sub add_pan_control {
	my $n = shift;
	
	my $pan_id = cop_add({
				chain => $n, 
				type => 'epp',
				cop_id => $ti[$n]->pan, # often undefined
				});
	
	$ti[$n]->set(pan => $pan_id);  # save the id for next time
	$pan_id;
}
## version functions


sub mon_vert {
	my $ver = shift;
	$tracker->set(version => $ver);
	$ui->refresh();
}
## chain setup generation


sub all_chains {
	my @active_tracks = grep { $_->rec_status ne q(OFF) } Audio::Ecasound::Multitrack::Track::all() 
		if Audio::Ecasound::Multitrack::Track::all();
	map{ $_->n} @active_tracks if @active_tracks;
}

sub user_rec_tracks {
	my @user_tracks = Audio::Ecasound::Multitrack::Track::all();
	splice @user_tracks, 0, 2; # drop Master and Mixdown tracks
	return unless @user_tracks;
	my @user_rec_tracks = grep { $_->rec_status eq 'REC' } @user_tracks;
	return unless @user_rec_tracks;
	map{ $_->n } @user_rec_tracks;
}
sub user_mon_tracks {
	my @user_tracks = Audio::Ecasound::Multitrack::Track::all();
	splice @user_tracks, 0, 2; # drop Master and Mixdown tracks
	return unless @user_tracks;
	my @user_mon_tracks = grep { $_->rec_status eq 'MON' } @user_tracks;
	return unless @user_mon_tracks;
	map{ $_->n } @user_mon_tracks;

}

sub really_recording {  # returns $output{file} entries

#	scalar @record  
	#print join "\n", "", ,"file recorded:", keys %{$outputs{file}}; # includes mixdown
# 	map{ s/ .*$//; $_}  # unneeded
	keys %{$outputs{file}}; # strings include format strings mixdown
}

sub write_chains {
	$debug2 and print "&write_chains\n";

	# $bus->apply;
	# $mixer->apply;
	# $ui->write_chains

	# we can assume that %inputs and %outputs will have the
	# same lowest-level keys
	#
	my @buses = grep { $_ ne 'file' and $_ ne 'device' } keys %inputs;
	
	### Setting devices as inputs (used by i.e. rec_setup)
	
	for my $dev (keys %{ $inputs{device} } ){

		$debug and print "dev: $dev\n";
		push  @input_chains, 
		join " ", "-a:" . (join ",", @{ $inputs{device}->{$dev} }),
			"-f:" .  $devices{$dev}->{input_format},
			"-i:" .  $devices{$dev}->{ecasound_id}, 
	}
	#####  Setting devices as outputs
	#
	for my $dev ( keys %{ $outputs{device} }){
			push @output_chains, join " ",
				"-a:" . (join "," , @{ $outputs{device}->{$dev} }),
				"-f:" . $devices{$dev}->{output_format},
				"-o:". $devices{$dev}->{ecasound_id};
	}
	### Setting loops as inputs 

	for my $bus( @buses ){ # i.e. 'mixed', 'cooked'
		for my $loop ( keys %{ $inputs{$bus} }){
			push  @input_chains, 
			join " ", 
				"-a:" . (join ",", @{ $inputs{$bus}->{$loop} }),
				"-i:$loop";
		}
	}
	### Setting loops as outputs 

	for my $bus( @buses ){ # i.e. 'mixed', 'cooked'
		for my $loop ( keys %{ $outputs{$bus} }){
			push  @output_chains, 
			join " ", 
				"-a:" . (join ",", @{ $outputs{$bus}->{$loop} }),
				"-o:$loop";
		}
	}
	##### Setting files as inputs (used by mon_setup)

	for my $full_path (keys %{ $inputs{file} } ) {
		
		$debug and print "monitor input file: $full_path\n";
		my $chain_ids = join ",",@{ $inputs{file}->{$full_path} };
		my ($chain) = $chain_ids =~ m/(\d+)/;
		$debug and print "input chain: $chain\n";
		push @input_chains, join ( " ",
					"-a:".$chain_ids,
			 		"-i:".  $Audio::Ecasound::Multitrack::ti[$chain]->modifiers .  $full_path);
 	}
	##### Setting files as outputs (used by rec_file and mix)

	for my $key ( keys %{ $outputs{file} } ){
		my ($full_path, $format) = split " ", $key;
		$debug and print "record output file: $full_path\n";
		my $chain_ids = join ",",@{ $outputs{file}->{$key} };
		
		push @output_chains, join ( " ",
			 "-a:".$chain_ids,
			 "-f:".$format,
			 "-o:".$full_path,
		 );
			 
			 
	}

	## write general options
	
	my $ecs_file = "# ecasound chainsetup file\n\n";
	$ecs_file   .= "# general\n\n";
	$ecs_file   .= "$ecasound_globals\n\n";
	$ecs_file   .= "# audio inputs\n\n";
	$ecs_file   .= join "\n", sort @input_chains;
	$ecs_file   .= "\n\n# post-input processing\n\n";
	$ecs_file   .= join "\n", sort map{ "-a:$_ $post_input{$_}"} keys %post_input;
	$ecs_file   .= "\n\n# pre-output processing\n\n";
	$ecs_file   .= join "\n", sort map{ "-a:$_ $pre_output{$_}"} keys %pre_output;
	$ecs_file   .= "\n\n# audio outputs";
	$ecs_file   .= join "\n", sort @output_chains, "\n";
	
	$debug and print "ECS:\n",$ecs_file;
	my $sf = join_path(&project_dir, $chain_setup_file);
	open ECS, ">$sf" or croak "can't open file $sf:  $!\n";
	print ECS $ecs_file;
	close ECS;


	# write .ewf files
	#
	map{ $_->write_ewf  } Audio::Ecasound::Multitrack::Track::all();
	
}

## transport functions

sub load_ecs {
		local $debug = 0;
		my $project_file = join_path(&project_dir , $chain_setup_file);
		eval_iam("cs-disconnect") if eval_iam("cs-connected");
		eval_iam("cs-remove $project_file");
		eval_iam("cs-load ". $project_file);
		$debug and map{print "$_\n\n"}map{$e->eci($_)} qw(cs es fs st ctrl-status);
}
sub new_engine { 
	my $ecasound  = $ENV{ECASOUND} ? $ENV{ECASOUND} : q(ecasound);
	#print "ecasound name: $ecasound\n";
	system qq(killall $ecasound);
	sleep 1;
	system qq(killall -9 $ecasound);
	$e = Audio::Ecasound->new();
}
sub generate_setup { # create chain setup
	remove_small_wavs();
	$debug2 and print "&generate_setup\n";
	%inputs = %outputs 
			= %post_input 
			= %pre_output 
			= @input_chains 
			= @output_chains 
			= ();
	my @tracks = Audio::Ecasound::Multitrack::Track::all;
	shift @tracks; # drop Master

	
	my $have_source = join " ", map{$_->name} 
								grep{ $_ -> rec_status ne 'OFF'} 
								@tracks;
	#print "have source: $have_source\n";
	if ($have_source) {
		$mixdown_bus->apply; # mix_file
		$master_bus->apply; # mix_out, mix_link

		## we want to apply 'multi' only to tracks with
		### with mon_ch defined, and $multi_enable on
		
		$tracker_bus->apply;
		map{ eliminate_loops($_) } all_chains();
		#print "minus loops\n \%inputs\n================\n", yaml_out(\%inputs);
		#print "\%outputs\n================\n", yaml_out(\%outputs);
		write_chains();
		return 1;
	} else { print "No inputs found!\n";
	return 0};
}

sub connect_transport {
	load_ecs(); 
	eval_iam("cs-selected") and	eval_iam("cs-is-valid")
		or print("Invalid chain setup, engine not ready.\n"),return;
	find_op_offsets(); 
	apply_ops();
	eval_iam('cs-connect');
	carp("Invalid chain setup, cannot arm transport.\n"), return 
		unless eval_iam("engine-status") eq 'not started' ;
	eval_iam('engine-launch');
	carp("Invalid chain setup, cannot arm transport.\n"), return
		unless eval_iam("engine-status") eq 'stopped' ;
	$length = eval_iam('cs-get-length'); 
	$ui->length_display(-text => colonize($length));
	# eval_iam("cs-set-length $length") unless @record;
	$ui->clock_config(-text => colonize(0));
	transport_status();
	$ui->flash_ready();
	#print eval_iam("fs");
	
}

sub transport_status {
	my $start  = Audio::Ecasound::Multitrack::Mark::loop_start();
	my $end    = Audio::Ecasound::Multitrack::Mark::loop_end();
	#print "start: $start, end: $end, loop_enable: $loop_enable\n";
	if ($loop_enable and $start and $end){
		#if (! $end){  $end = $start; $start = 0}
		print "looping from ", d1($start), 
			($start > 120 
				? " (" . colonize( $start ) . ") "  
				: " " ),
						"to ", d1($end),
			($end > 120 
				? " (".colonize( $end ). ") " 
				: " " ),
				$/;
	}
	print "setup length is ", d1($length), 
		($length > 120	?  " (" . colonize($length). ")" : "" )
		,$/;
	print "now at ", colonize( eval_iam( "getpos" )), $/;
	print "engine is ", eval_iam("engine-status"), $/;
}
sub start_transport { 
	$debug2 and print "&start_transport\n";
	carp("Invalid chain setup, aborting start.\n"),return unless eval_iam("cs-is-valid");
	#
	# we are going to have a heartbeat function.
	# It will wakeup every three seconds
	# will do several jobs, one is to calculate
	# the time till the replay, then if that
	# time is less than 6s, the wraparound will be
	# scheduled.
	#
	# if the stop button is pressed, we cancel
	#
	#
	#carp "transport appears stuck: ",eval_iam("engine-status"),$/;
	#if twice (or 3x in a row) not running status, 

	print "starting at ", colonize(int (eval_iam "getpos")), $/;
	eval_iam('start');
	$ui->start_heartbeat();

	sleep 1; # time for engine
	print "engine is ", eval_iam("engine-status"), $/;
}
sub start_heartbeat {
	$event_id{heartbeat} = $new_event->repeat( 3000,
				sub { 
				
				my $here   = eval_iam("getpos");
				my $status = eval_iam q(engine-status);
				$new_event->afterCancel($event_id{heartbeat})
					#if $status =~ /finished|error|stopped/;
					if $status =~ /finished|error/;
				print join " ", "engine is $status", colonize($here), $/;
				my ($start, $end);
				$start  = Audio::Ecasound::Multitrack::Mark::loop_start();
				$end    = Audio::Ecasound::Multitrack::Mark::loop_end();
				schedule_wraparound() 
					if $loop_enable 
					and defined $start 
					and defined $end 
					and !  really_recording();
				update_clock();

				});

}

sub schedule_wraparound {
	my $here   = eval_iam("getpos");
	my $start  = Audio::Ecasound::Multitrack::Mark::loop_start();
	my $end    = Audio::Ecasound::Multitrack::Mark::loop_end();
	my $diff = $end - $here;
	$debug and print "here: $here, start: $start, end: $end, diff: $diff\n";
	if ( $diff < 0 ){ # go at once
		eval_iam("setpos ".$start);
	} elsif ( $diff < 6 ) { #schedule the move
	$event_id{wraparound} = $new_event->after( 
		int( $diff*1000 ), sub{ eval_iam("setpos " . $start) } )
		
		unless $event_id{wraparound};
		
		;
	}
}

	
sub prepare_looping {
	# print "looping enabled\n";
	my $here   = eval_iam q(getpos), 
	my $end    = Audio::Ecasound::Multitrack::Mark::loop_end();
	my $start  = Audio::Ecasound::Multitrack::Mark::loop_start();
	my $diff = $end - $here;
	$debug and print "here: $here, start: $start, end: $end, diff: $diff\n";
	if ( $diff < 0 ){
		eval_iam("setpos ".$start);
		sleep 1;
		prepare_looping();
	} else {
		$event_id{loop} =  $new_event->after(
			int($diff * 1000), sub {
				eval_iam("setpos ".$start) ;
				sleep 1;
				prepare_looping();
			}
		);
	}
		#   will need to cancel on transport stop
}
sub stop_transport { 
	$debug2 and print "&stop_transport\n"; 
	map{ $new_event->afterCancel($event_id{$_})} qw(heartbeat wraparound);
	eval_iam('stop');	
	print "engine is ", eval_iam("engine-status"), $/;
	$ui->project_label_configure(-background => $old_bg);
	rec_cleanup();
}
sub transport_running {
#	$debug2 and print "&transport_running\n";
	 eval_iam('engine-status') eq 'running' ;
}
sub disconnect_transport {
	return if transport_running();
		eval_iam("cs-disconnect") if eval_iam("cs-connected");
}


sub toggle_unit {
	if ($unit == 1){
		$unit = 60;
		
	} else{ $unit = 1; }
}
sub show_unit { $time_step->configure(
	-text => ($unit == 1 ? 'Sec' : 'Min') 
)}

# GUI routines
sub drop_mark {
	my $here = eval_iam("cs-get-position");
	return if grep { $_->time == $here } Audio::Ecasound::Multitrack::Mark::all();
	my $mark = Audio::Ecasound::Multitrack::Mark->new( time => $here );
		$ui->marker($mark); # for GUI
}
sub mark {
	my $mark = shift;
	my $pos = $mark->time;
	if ($markers_armed){ 
			$ui->destroy_marker($pos);
			$mark->remove;
		    arm_mark_toggle(); # disarm
	}
	else{ 

		eval_iam(qq(cs-set-position $pos));
	}
}

# TEXT routines


sub next_mark {
	my $jumps = shift;
	$jumps and $jumps--;
	my $here = eval_iam("cs-get-position");
	my @marks = sort { $a->time <=> $b->time } @Audio::Ecasound::Multitrack::Mark::all;
	for my $i ( 0..$#marks ){
		if ($marks[$i]->time - $here > 0.001 ){
			$debug and print "here: $here, future time: ",
			$marks[$i]->time, $/;
			eval_iam("setpos " .  $marks[$i+$jumps]->time);
			$this_mark = $marks[$i];
			return;
		}
	}
}
sub previous_mark {
	my $jumps = shift;
	$jumps and $jumps--;
	my $here = eval_iam("cs-get-position");
	my @marks = sort { $a->time <=> $b->time } @Audio::Ecasound::Multitrack::Mark::all;
	for my $i ( reverse 0..$#marks ){
		if ($marks[$i]->time < $here ){
			eval_iam("setpos " .  $marks[$i+$jumps]->time);
			$this_mark = $marks[$i];
			return;
		}
	}
}
	

## clock and clock-refresh functions ##
#

## jump recording head position

sub to_start { 
	return if really_recording();
	eval_iam(qq(cs-set-position 0));
}
sub to_end { 
	# ten seconds shy of end
	return if really_recording();
	my $end = eval_iam(qq(cs-get-length)) - 10 ;  
	eval_iam(qq(cs-set-position $end));
} 
sub jump {
	return if really_recording();
	my $delta = shift;
#	my $running = eval_iam("engine-status") eq 'running' ?  1 : 0;
#	eval_iam "stop"; #  if $running;
	$debug2 and print "&jump\n";
	my $here = eval_iam(qq(getpos));
	$debug and print "delta: $delta\nhere: $here\nunit: $unit\n\n";
	my $new_pos = $here + $delta * $unit;
	$new_pos = $new_pos < $length ? $new_pos : $length - 10;
	# eval_iam("setpos $new_pos");
	my $cmd = "setpos $new_pos";
	$e->eci("setpos $new_pos");
	# print "$cmd\n";
	# eval_iam "start" if $running;
	sleep 1;
}
## post-recording functions

sub rec_cleanup {  
	$debug2 and print "&rec_cleanup\n";
	return if transport_running();
 	my @k = really_recording();
	$debug and print "found files: " , join $/, @k;
	return unless @k;
	print "I was recording!\n";
	my $recorded = 0;
 	for my $k (@k) {    
 		my ($n) = $outputs{file}{$k}[-1] =~ m/(\d+)/; 
		print "k: $k, n: $n\n";
		my $file = $k;
		$file =~ s/ .*$//;
 		my $test_wav = $file;
		$debug and print "track: $n, file: $test_wav\n";
 		my ($v) = ($test_wav =~ /_(\d+)\.wav$/); 
		$debug and print "n: $n\nv: $v\n";
		$debug and print "testing for $test_wav\n";
		if (-e $test_wav) {
			$debug and print "exists. ";
			if (-s $test_wav > 44100) { # 0.5s x 16 bits x 44100/s
				$debug and print "bigger than a breadbox.  \n";
				#$ti[$n]->set(active => $ti[$n]->last); 
				$ui->update_version_button($n, $v);
			$recorded++;
			}
			else { unlink $test_wav }
		}
	}
	my $mixed = scalar ( grep{ /\bmix*.wav/i} @k );
	
	$debug and print "recorded: $recorded mixed: $mixed\n";
	if ( ($recorded -  $mixed) >= 1) {
			# i.e. there are first time recorded tracks
			#$ui->update_master_version_button();
			$ui->global_version_buttons(); # recreate
			$tracker->set( rw => 'MON');
			generate_setup() and connect_transport();
			$ui->refresh();
	}
		
} 
## effect functions
sub add_effect {
	local $debug = 0;
	
	$debug2 and print "&add_effect\n";
	
	my %p 			= %{shift()};
	my $n 			= $p{chain};
	my $code 			= $p{type};
	my $parent_id = $p{parent_id};  
	my $id		= $p{cop_id};   # initiates restore
	my $parameter		= $p{parameter}; 
	my $i = $effect_i{$code};
	my $values = $p{values};

	return if $id eq $ti[$n]->vol or
	          $id eq $ti[$n]->pan;   # skip these effects 
			   								# already created in add_track

	$id = cop_add(\%p); 
	my %pp = ( %p, cop_id => $id); # replace chainop id
	$ui->add_effect_gui(\%pp);
	apply_op($id) if eval_iam("cs-is-valid");

}

sub remove_effect {
	local $debug = 1;
	@_ = discard_object(@_);
	$debug2 and print "&remove_effect\n";
	my $id = shift;
	my $n = $cops{$id}->{chain};
	$ti[$n]->remove_effect( $id );
		
	$debug and print "ready to remove cop_id: $id\n";

	# if i belong to someone remove their ownership of me

	if ( my $parent = $cops{$id}->{belongs_to} ) {
	$debug and print "parent $parent owns list: ", join " ",
		@{ $cops{$parent}->{owns} }, "\n";

	@{ $cops{$parent}->{owns} }  =  grep{ $_ ne $id}
		@{ $cops{$parent}->{owns} } ; 
	$cops{$id}->{belongs_to} = undef;
	$debug and print "parent $parent new owns list: ", join " ",
	}

	# recursively remove children
	$debug and print "children found: ", join "|",@{$cops{$id}->{owns}},"\n";
		
	# parameter controllers are not separate ops
	map{remove_effect($_)}@{ $cops{$id}->{owns} };

	
	# remove my own cop_id from the stack
	$ui->remove_effect_gui($id), remove_op($id)  unless $cops{$id}->{belongs_to};
	
			
}
sub remove_effect_gui { 
	@_ = discard_object(@_);
	$debug2 and print "&remove_effect_gui\n";
	my $id = shift;
	my $n = $cops{$id}->{chain};
	$debug and print "id: $id, chain: $n\n";

	$ti[$n]->set(ops =>  
		[ grep{ $_ ne $id} @{ $ti[ $cops{$id}->{chain} ]->ops } ]);
	$debug and print "i have widgets for these ids: ", join " ",keys %effects_widget, "\n";
	$debug and print "preparing to destroy: $id\n";
	$effects_widget{$id}->destroy();
	delete $effects_widget{$id}; 

}

sub remove_op {

	my $id = shift;
	my $n = $cops{$id}->{chain};
	if ( $cops{$id}->{belongs_to}) { 
		return;
	}
	my $index; 
	$debug and print "ops list for chain $n: @{$ti[$n]->ops}\n";
	$debug and print "operator id to remove: $id\n";
		for my $pos ( 0.. scalar @{ $ti[$n]->ops } - 1  ) {
			($index = $pos), last if $ti[$n]->ops->[$pos] eq $id; 
		};
	$debug and print "ready to remove from chain $n, operator id $id, index $index\n";
	$debug and eval_iam ("cs");
	 eval_iam ("c-select $n");
	eval_iam ("cop-select ". ($ti[$n]->offset + $index));
	eval_iam ("cop-remove");
	$debug and eval_iam ("cs");

	delete $cops{$id};
	delete $copp{$id};
}
sub cop_add {
	my %p 			= %{shift()};
	my $n 			= $p{chain};
	my $code		= $p{type};
	my $parent_id = $p{parent_id};  
	my $id		= $p{cop_id};   # causes restore behavior when present
	my $i       = $effect_i{$code};
	my @values = @{ $p{values} } if $p{values};
	my $parameter	= $p{parameter};  # needed for parameter controllers
	$debug2 and print "&cop_add\n";
$debug and print <<PP;
n:          $n
code:       $code
parent_id:  $parent_id
cop_id:     $id
effect_i:   $i
parameter:  $parameter
PP

	return $id if $id; # do nothing if cop_id has been issued

	# make entry in %cops with chain, code, display-type, children

	$debug and print "Issuing a new cop_id for track $n: $cop_id\n";
	# from the cop_id, we may also need to know chain number and effect

	$cops{$cop_id} = {chain => $n, 
					  type => $code,
					  display => $effects[$i]->{display},
					  owns => [] }; # DEBUGGIN TEST

	$p{cop_id} = $cop_id;
 	cop_init ( \%p );

	if ($parent_id) {
		$debug and print "parent found: $parent_id\n";

		# store relationship
		$debug and print "parent owns" , join " ",@{ $cops{$parent_id}->{owns}}, "\n";

		push @{ $cops{$parent_id}->{owns}}, $cop_id;
		$debug and print join " ", "my attributes:", (keys %{ $cops{$cop_id} }), "\n";
		$cops{$cop_id}->{belongs_to} = $parent_id;
		$debug and print join " ", "my attributes again:", (keys %{ $cops{$cop_id} }), "\n";
		$debug and print "parameter: $parameter\n";
		$copp{$cop_id}->[0] = $parameter + 1; # set fx-param to the parameter number.
 		# find position of parent and insert child immediately afterwards

 		my $end = scalar @{ $ti[$n]->ops } - 1 ; 
 		for my $i (0..$end){
 			splice ( @{$ti[$n]->ops}, $i+1, 0, $cop_id ), last
 				if $ti[$n]->ops->[$i] eq $parent_id 
 		}
	}
	else { push @{$ti[$n]->ops }, $cop_id; } 

	# set values if present
	
	$copp{$cop_id} = \@values if @values; # needed for text mode

	$cop_id++; # return value then increment
}

sub cop_init {
	
	$debug2 and print "&cop_init\n";
	my $p = shift;
	my %p = %$p;
	my $id = $p{cop_id};
	my $parent_id = $p{parent_id};
	my $vals_ref  = $p{vals_ref};
	
	$debug and print "cop__id: $id\n";

	my @vals;
	if (ref $vals_ref) {
	# untested
		@vals = @{ $vals_ref };
		$debug and print ("values supplied\n");
		@{ $copp{$id} } = @vals;
		return;
	} 
	else { 
		$debug and print "no settings found, loading defaults if present\n";
		my $i = $effect_i{ $cops{$id}->{type} };
		
		# CONTROLLER
		# don't initialize first parameter if operator has a parent
		# i.e. if operator is a controller
		for my $p ($parent_id ? 1 : 0..$effects[$i]->{count} - 1) {
		#TODO  support controller-type operators
		
		# for my $p (0..$effects[$i]->{count} - 1) {
			my $default = $effects[$i]->{params}->[$p]->{default};
			push @vals, $default;
		}
		@{ $copp{$id} } = @vals;
		$debug and print "copid: $id defaults: @vals \n";
	}
}

sub sync_effect_param {
	my ($id, $param) = @_;

	effect_update( $cops{$id}{chain}, 
					$id, 
					$param, 
					$copp{$id}[$param]	 );
}

sub effect_update_copp_set {
	# will superseded effect_update for most places
	my ($chain, $id, $param, $val) = @_;
	effect_update( @_ );
	$copp{$id}->[$param] = $val;
}
	
	
sub effect_update {
	
	# why not use this routine to update %copp values as
	# well?
	
	local $debug = 0;
	my $es = eval_iam "engine-status";
	$debug and print "engine is $es\n";
	return if $es !~ /not started|stopped|running/;

	my ($chain, $id, $param, $val) = @_;

	# $param gets incremented, therefore is zero-based. 
	# if I check i will find %copp is  zero-based

	$debug2 and print "&effect_update\n";
	return if $ti[$chain]->rec_status eq "OFF"; 
	return if $ti[$chain]->name eq 'Mixdown' and 
			  $ti[$chain]->rec_status eq 'REC';
 	$debug and print join " ", @_, "\n";	

	# update Ecasound's copy of the parameter

	$debug and print "valid: ", eval_iam("cs-is-valid"), "\n";
	my $controller; 
	for my $op (0..scalar @{ $ti[$chain]->ops } - 1) {
		$ti[$chain]->ops->[$op] eq $id and $controller = $op;
	}
	$param++; # so the value at $p[0] is applied to parameter 1
	$controller++; # translates 0th to chain-operator 1
	$debug and print 
	"cop_id $id:  track: $chain, controller: $controller, offset: ",
	$ti[$chain]->offset, " param: $param, value: $val$/";
	eval_iam ("c-select $chain");
	eval_iam ("cop-select ". ($ti[$chain]->offset + $controller));
	eval_iam ("copp-select $param");
	eval_iam ("copp-set $val");
}
sub find_op_offsets {

	
	$debug2 and print "&find_op_offsets\n";
	eval_iam('c-select-all');
		#my @op_offsets = split "\n",eval_iam("cs");
		my @op_offsets = grep{ /"\d+"/} split "\n",eval_iam("cs");
		shift @op_offsets; # remove comment line
		$debug and print join "\n\n",@op_offsets; 
		for my $output (@op_offsets){
			my $chain_id;
			($chain_id) = $output =~ m/Chain "(\w*\d+)"/;
			# print "chain_id: $chain_id\n";
			next if $chain_id =~ m/\D/; # skip id's containing non-digits
										# i.e. M1
			my $quotes = $output =~ tr/"//;
			$debug and print "offset: $quotes in $output\n"; 
			$ti[$chain_id]->set( offset => $quotes/2 - 1);  

		}
}
sub apply_ops {  # in addition to operators in .ecs file
	
	$debug2 and print "&apply_ops\n";
	my $last = scalar @Audio::Ecasound::Multitrack::Track::by_index - 1;
	$debug and print "looping over 1 to $last\n";
	for my $n (1..$last) {
	$debug and print "chain: $n, offset: ", $ti[$n]->offset, "\n";
 		next if $ti[$n]->rec_status eq "OFF" ;
		#next if $n == 2; # no volume control for mix track
		#next if ! defined $ti[$n]->offset; # for MIX
 		#next if ! $ti[$n]->offset ;
		for my $id ( @{ $ti[$n]->ops } ) {
		#	next if $cops{$id}->{belongs_to}; 
		apply_op($id);
		}
	}
}
sub apply_op {
	$debug2 and print "&apply_op\n";
	
	my $id = shift;
	$debug and print "id: $id\n";
	my $code = $cops{$id}->{type};
	$debug and print "chain: $cops{$id}->{chain} type: $cops{$id}->{type}, code: $code\n";
	#  if code contains colon, then follow with comma (preset, LADSPA)
	#  if code contains no colon, then follow with colon (ecasound,  ctrl)
	
	$code = '-' . $code . ($code =~ /:/ ? q(,) : q(:) );
	my @vals = @{ $copp{$id} };
	$debug and print "values: @vals\n";

	# we start to build iam command

	
	my $add = "cop-add "; 
	$add .= $code . join ",", @vals;

	# if my parent has a parent then we need to append the -kx  operator

	my $dad = $cops{$id}->{belongs_to};
	$add .= " -kx" if $cops{$dad}->{belongs_to};
	$debug and print "operator:  ", $add, "\n";

	eval_iam ("c-select $cops{$id}->{chain}") 
		unless $cops{$id}->{belongs_to}; # avoid reset
	eval_iam ($add);
	$debug and print "children found: ", join ",", "|",@{$cops{$id}->{owns}},"|\n";
	my $ref = ref $cops{$id}->{owns} ;
	$ref =~ /ARRAY/ or croak "expected array";
	my @owns = @{ $cops{$id}->{owns} };
	$debug and print "owns: @owns\n";  
	map{apply_op($_)} @owns;

}
## static effects data



# @ladspa_sorted # 

sub prepare_static_effects_data{
	
	$debug2 and print "&prepare_static_effects_data\n";

	my $effects_cache = join_path(&project_root, $effects_cache_file);

	# TODO re-read effects data if ladspa or user presets are
	# newer than cache

	if (-f $effects_cache and ! $opts{s}){  
		$debug and print "found effects cache: $effects_cache\n";
		assign_var($effects_cache, @effects_static_vars);
	} else {
		
		$debug and print "reading in effects data, please wait...\n";
		read_in_effects_data(); 
		get_ladspa_hints();
		integrate_ladspa_hints();
		sort_ladspa_effects();
		serialize (
			-file => $effects_cache, 
			-vars => \@effects_static_vars,
			-class => 'Audio::Ecasound::Multitrack',
			-storable => 1 );
	}

	prepare_effect_index();
}
sub prepare_effect_index {
	%effect_j = ();
=comment
	my @ecasound_effects = qw(
		ev evp ezf eS ea eac eaw eal ec eca enm ei epp
		ezx eemb eemp eemt ef1 ef3 ef4 efa efb efc efh efi
		efl efr efs erc erm etc etd ete etf etl etm etp etr);
	map { $effect_j{$_} = $_ } @ecasound_effects;
=cut
	map{ 
		my $code = $_;
		my ($short) = $code =~ /:(\w+)/;
		if ( $short ) { 
			if ($effect_j{$short}) { warn "name collision: $_\n" }
			else { $effect_j{$short} = $code }
		}else{ $effect_j{$code} = $code };
	} keys %effect_i;
	#print yaml_out \%effect_j;
}
sub extract_effects_data {
	my ($lower, $upper, $regex, $separator, @lines) = @_;
	carp ("incorrect number of lines ", join ' ',$upper-$lower,scalar @lines)
		if $lower + @lines - 1 != $upper;
	$debug and print"lower: $lower upper: $upper  separator: $separator\n";
	#$debug and print "lines: ". join "\n",@lines, "\n";
	$debug and print "regex: $regex\n";
	
	for (my $j = $lower; $j <= $upper; $j++) {
		my $line = shift @lines;
	
		$line =~ /$regex/ or carp("bad effect data line: $line\n"),next;
		my ($no, $name, $id, $rest) = ($1, $2, $3, $4);
		$debug and print "Number: $no Name: $name Code: $id Rest: $rest\n";
		my @p_names = split $separator,$rest; 
		map{s/'//g}@p_names; # remove leading and trailing q(') in ladspa strings
		$debug and print "Parameter names: @p_names\n";
		$effects[$j]={};
		$effects[$j]->{number} = $no;
		$effects[$j]->{code} = $id;
		$effects[$j]->{name} = $name;
		$effects[$j]->{count} = scalar @p_names;
		$effects[$j]->{params} = [];
		$effects[$j]->{display} = qq(field);
		map{ push @{$effects[$j]->{params}}, {name => $_} } @p_names;
	}
}
sub sort_ladspa_effects {
	$debug2 and print "&sort_ladspa_effects\n";
#	print yaml_out(\%e_bound); 
	my $aa = $e_bound{ladspa}{a};
	my $zz = $e_bound{ladspa}{z};
#	print "start: $aa end $zz\n";
	map{push @ladspa_sorted, 0} ( 1 .. $aa ); # fills array slice [0..$aa-1]
	splice @ladspa_sorted, $aa, 0,
		 sort { $effects[$a]->{name} cmp $effects[$b]->{name} } ($aa .. $zz) ;
	$debug and print "sorted array length: ". scalar @ladspa_sorted, "\n";
}		
sub read_in_effects_data {

	local $debug = 0;
	$debug2 and print "&read_in_effects_data\n";
	read_in_tkeca_effects_data();

	# read in other effects data
	
	my $lr = eval_iam("ladspa-register");

	#print $lr; 
	
	my @ladspa =  split "\n", $lr;

	
	#$lr > io("lr");
	#split /\n+/, 
	
	# grep {! /^\w*$/ } 
	
	# join the two lines of each entry
	my @lad = map { join " ", splice(@ladspa,0,2) } 1..@ladspa/2; 

	my @preset = grep {! /^\w*$/ } split "\n", eval_iam("preset-register");
	my @ctrl  = grep {! /^\w*$/ } split "\n", eval_iam("ctrl-register");


#	print eval_iam("ladspa-register");
	
	$debug and print "found ", scalar @lad, " LADSPA effects\n";
	$debug and print "found ", scalar @preset, " presets\n";
	$debug and print "found ", scalar @ctrl, " controllers\n";

	# index boundaries we need to make effects list and menus

	$e_bound{ladspa}{a} = $e_bound{tkeca}{z} + 1;
	$e_bound{ladspa}{b} = $e_bound{tkeca}{z} + int(@lad/4);
	$e_bound{ladspa}{c} = $e_bound{tkeca}{z} + 2*int(@lad/4);
	$e_bound{ladspa}{d} = $e_bound{tkeca}{z} + 3*int(@lad/4);
	$e_bound{ladspa}{z} = $e_bound{tkeca}{z} + @lad;
	$e_bound{preset}{a} = $e_bound{ladspa}{z} + 1;
	$e_bound{preset}{b} = $e_bound{ladspa}{z} + int(@preset/2);
	$e_bound{preset}{z} = $e_bound{ladspa}{z} + @preset;
	$e_bound{ctrl}{a}   = $e_bound{preset}{z} + 1;
	$e_bound{ctrl}{z}   = $e_bound{preset}{z} + @ctrl;

	my $preset_re = qr/
		^(\d+) # number
		\.    # dot
		\s+   # spaces+
		(\w+) # name
		,\s*  # comma spaces* 
		-(pn:\w+)    # preset_id 
		:?     # maybe colon (if parameters)
		(.*$)  # rest
	/x;

	my $ladspa_re = qr/
		^(\d+) # number
		\.    # dot
		\s+  # spaces
		(\w.+?) # name, starting with word-char,  non-greedy
		\s+     # spaces
		-(el:\w+),? # ladspa_id maybe followed by comma
		(.*$)        # rest
	/x;

	my $ctrl_re = qr/
		^(\d+) # number
		\.     # dot
		\s+    # spaces
		(\w.+?) # name, starting with word-char,  non-greedy
		,\s*    # comma, zero or more spaces
		-(k\w+):?    # ktrl_id maybe followed by colon
		(.*$)        # rest
	/x;

	extract_effects_data(
		$e_bound{ladspa}{a},
		$e_bound{ladspa}{z},
		$ladspa_re,
		q(','),
		@lad,
	);

	extract_effects_data(
		$e_bound{preset}{a},
		$e_bound{preset}{z},
		$preset_re,
		q(,),
		@preset,
	);
	extract_effects_data(
		$e_bound{ctrl}{a},
		$e_bound{ctrl}{z},
		$ctrl_re,
		q(,),
		@ctrl,
	);



	for my $i (0..$#effects){
		 $effect_i{ $effects[$i]->{code} } = $i; 
		 $debug and print "i: $i code: $effects[$i]->{code} display: $effects[$i]->{display}\n";
	}

	$debug and print "\@effects\n======\n", yaml_out(\@effects); ; 
}
sub read_in_tkeca_effects_data {

# Based on GPL code in Tkeca

# controller (effect) data format
# code|name|number_of_parameters| ( Label|scale_start|scale_end|default|resolution ) x number_of_parameters

# I left the tcl code 'as is' in the following pasted section, using regexes 
# so future updates from him can be pasted in without editing.

# divide by lines, remove stuff outside quotes, 
# then make an anonymous array of the fields of each line

	my @effects_data = 	map { [split /\|/, $_ ]  }  
						map{ s/^.*?"//; s/"\s*$//; $_} 
						split "\n",$tkeca_effects_data; 
	
	$e_bound{tkeca}{a}  = 1;
	$e_bound{tkeca}{z}  = scalar @effects_data;  

	for my $i (1..@effects_data){
		my @row = @{ shift @effects_data };
		@{$effects[$i]}{ qw(code name count) } = splice @row, 0, 3;

		# default display format

		$effects[$i]->{display} = qq(scale);

	# maps effect code (i.e. epp) to an index in array holding static effects data
	#print "effects code: $i stands for ", $effects[$i]->{code}, "\n";
	#print "count: $effects[$i]->{count}\n";

			for (1..$effects[$i]->{count}){
				my %p;
				#print join " / ",splice (@row, 0,5), "\n";
				@p{ qw(name begin end default resolution) }  =  splice @row, 0, 5;
				# print "\%p\n======\n", yaml_out(\%p);
				push @{$effects[$i]->{params}}, \%p;

			}
	}

}
sub get_ladspa_hints{
	$debug2 and print "&get_ladspa_hints\n";
	$ENV{LADSPA_PATH} or local $ENV{LADSPA_PATH}='/usr/lib/ladspa';
	my @dirs =  split ':', $ENV{LADSPA_PATH};
	my $data = '';
	for my $dir (@dirs) {
		opendir DIR, $dir or carp qq(can't open LADSPA dir "$dir" for read: $!\n);
		my @plugins = grep{ /\.so$/ } readdir DIR;
		$data .= join "", map { `analyseplugin $_` } @plugins;
		closedir DIR;
	}
	# print $data; exit;
	my @plugin_stanzas = split "\n\n\n", $data;
	# print scalar @plugin_stanzas; exit;
	# print $data;

	# print "@plugins"; exit;
	# | perl -ne 'chomp; s/$ENV{LADSPA_PATH}//; system qq(analyseplugin $_)'
	my $ladspa_sample_rate = 44100; # for sample-rate dependent effect
	use Data::Dumper;

	my $pluginre = qr/
	Plugin\ Name: \s+ "([^"]+)" \s+
	Plugin\ Label:\s+ "([^"]+)" \s+
	[^\x00]+(?=Ports) 		# swallow maximum up to Ports
	Ports: \s+ ([^\x00]+) 	# swallow all
	/x;

	my $paramre = qr/
	"([^"]+)"   #  name inside quotes
	\s+
	(.+)        # rest
	/x;



	for my $stanza (@plugin_stanzas) {

		$stanza =~ /$pluginre/ or carp "*** couldn't match plugin stanza $stanza ***";

		my ($plugin_name, $plugin_label, $ports) = ($1, $2, $3);
		#print "$1\n$2\n$3"; exit;

		 my @lines = split "\n",$ports;
	#	print join "\n",@lines; exit;
		my @params;  # data

		my @names;
		for my $p (@lines) {
			next if $p =~ /^\s*$/;
			$p =~ /$paramre/;
			my ($name, $rest) = ($1, $2);
			my ($dir, $type, $range, $default, $hint) = split /\s*,\s*/ , $rest, 5;
			#print join "|",$dir, $type, $range, $default, $hint;
			next if $type eq q(audio);
			my %p;
			$p{name} = $name;
			$p{dir} = $dir;
			$p{hint} = $hint;
			my ($beg, $end, $default_val, $resolution) = range($name, $range, $default, $hint);
			$p{begin} = $beg;
			$p{end} = $end;
			$p{default} = $default_val;
			$p{resolution} = $resolution;
			push @params, { %p };
		}

		$plugin_label = "el:" . $plugin_label;
		$effects_ladspa {$plugin_label}->{params} = [ @params ];
		$effects_ladspa {$plugin_label}->{count} = scalar @params;
		$effects_ladspa {$plugin_label}->{display} = 'scale';
	}

	$debug and print yaml_out(\%effects_ladspa); 
}
sub range {
	my ($name, $range, $default, $hint) = @_; 
	my $multiplier = 1;;
	#$multiplier = $ladspa_sample_rate if $range =~ s/\*srate//g;
	$multiplier = $ladspa_sample_rate if $range =~ s/\*\s*srate//g;
	my ($beg, $end) = split /\s+to\s+/, $range;
	# if end is '...' set to $default + 10dB or $default * 10
	$default =~ s/default\s+//;
	$end =~ /\.{3}/ and $end = (
		$default == 0 ? 10  # '0' is probably 0db, so 0+10db
					  : $default * 10
		);
	$debug and print "1 beg: $beg  end: $end\n";
	$beg = $beg * $multiplier;
	$end = $end * $multiplier;
	$debug and print "2 beg: $beg  end: $end\n";

	my $resolution = ($end - $beg) / 100;
	if    ($hint =~ /integer/ ) { $resolution = 1; }
	elsif ($hint =~ /logarithmic/ ) {
		$beg = 0.0001 * $multiplier if ! $beg;
		$beg = round ( log $beg );
		$end = round ( log $end );
		$resolution = ($end - $beg) / 100;
		$default = round (log $default);
	}
	
	$resolution = d2( $resolution + 0.002) if $resolution < 1  and $resolution > 0.01;
	$resolution = dn ( $resolution, 3 ) if $resolution < 0.01;
	$resolution = int ($resolution + 0.1) if $resolution > 1 ;
	
	#print "3 beg: $beg  end: $end\n";
	($beg, $end, $default, $resolution)

}
sub integrate_ladspa_hints {
	map{ 
		my $i = $effect_i{$_};
		# print ("$_ not found\n"), 
		next unless $i;
		$effects[$i]->{params} = $effects_ladspa{$_}->{params};
		$effects[$i]->{display} = $effects_ladspa{$_}->{display};
	} keys %effects_ladspa;

my %L;
my %M;

map { $L{$_}++ } keys %effects_ladspa;
map { $M{$_}++ } grep {/el:/} keys %effect_i;

for my $k (keys %L) {
	$M{$k} or $debug and print "$k not found in ecasound listing\n";
}
for my $k (keys %M) {
	$L{$k} or $debug and print "$k not found in ladspa listing\n";
}


$debug and print join "\n", sort keys %effects_ladspa;
$debug and print '-' x 60, "\n";
$debug and print join "\n", grep {/el:/} sort keys %effect_i;

#print yaml_out \@effects; exit;

}
sub d1 {
	my $n = shift;
	sprintf("%.1f", $n)
}
sub d2 {
	my $n = shift;
	sprintf("%.2f", $n)
}
sub dn {
	my ($n, $places) = @_;
	sprintf("%." . $places . "f", $n);
}
sub round {
	my $n = shift;
	return 0 if $n == 0;
	$n = int $n if $n > 10;
	$n = d2($n) if $n < 10;
	$n;
}
	

## persistent state support

sub save_state {

	$debug2 and print "&save_state\n";
	my $file = shift;

	# remove nulls in %cops 
	delete $cops{''};

	map{ 
		my $found; 
		$found = "yes" if @{$cops{$_}->{owns}};
		$cops{$_}->{owns} = '~' unless $found;
	} keys %cops;

	# restore muted volume levels
	#
	my %muted;
	map{ $copp{ $ti[$_]->vol }->[0] = $old_vol{$_} ; 
		 $muted{$_}++;
	#	 $ui->paint_button($track_widget{$_}{mute}, q(brown) );
		} grep { $old_vol{$_} } all_chains();
	# TODO: old_vol should be incorporated into Track object
	# not separate variable
	#
	# (done for Text mode)

 # old vol level has been stored, thus is muted
	$file = $file ? $file : $state_store_file;
	$file = join_path(&project_dir, $file);
	# print "filename base: $file\n";
	print "saving state as $file\n";

    # sort marks
	
	my @marks = sort keys %marks;
	%marks = ();
	map{ $marks{$_}++ } @marks;
	
# prepare tracks for storage

@tracks_data = (); # zero based, iterate over these to restore

map { push @tracks_data, $_->hashref } Audio::Ecasound::Multitrack::Track::all();

# print "found ", scalar @tracks_data, "tracks\n";

# prepare marks data for storage (new Mark objects)

@marks_data = ();
map { push @marks_data, $_->hashref } Audio::Ecasound::Multitrack::Mark::all();

@groups_data = ();
map { push @groups_data, $_->hashref } Audio::Ecasound::Multitrack::Group::all();

	serialize(
		-file => $file, 
		-vars => \@persistent_vars,
		-class => 'Audio::Ecasound::Multitrack',
	#	-storable => 1,
		);


# store alsa settings

	if ( $opts{a} ) {
		my $file = $file;
		$file =~ s/\.yml$//;
		print "storing ALSA settings\n";
		print qx(alsactl -f $file.alsa store);
	}
	# now remute
	
	map{ $copp{ $ti[$_]->vol }->[0] = 0} 
	grep { $muted{$_}} 
	all_chains();

	# restore %cops
	map{ $cops{$_}->{owns} eq '~' and $cops{$_}->{owns} = [] } keys %cops; 

}
sub assign_var {
	my ($source, @vars) = @_;
	assign_vars(
				-source => $source,
				-vars   => \@vars,
				-class => 'Audio::Ecasound::Multitrack');
}
sub retrieve_state {
	$debug2 and print "&retrieve_state\n";
	my $file = shift;
	$file = $file ? $file : $state_store_file;
	$file = join_path(project_dir(), $file);
	my $yamlfile = $file;
	$yamlfile .= ".yml" unless $yamlfile =~ /yml$/;
	$file = $yamlfile if -f $yamlfile;
	! -f $file and print ("file not found: $file\n"), return;
	$debug and print "using file: $file";

	assign_var( $file, @persistent_vars );

	##  print yaml_out \@groups_data; 
	# %cops: correct 'owns' null (from YAML) to empty array []
	
	map{ $cops{$_}->{owns} or $cops{$_}->{owns} = [] } keys %cops; 

	#  set group parameters

	map {my $g = $_; 
		map{
			$Audio::Ecasound::Multitrack::Group::by_index[$g->{n}]->set($_ => $g->{$_})
			} keys %{$g};
	} @groups_data;

	#  set Master and Mixdown parmeters
	


	map {my $t = $_; 
			my %track = %{$t};
		map{

			$Audio::Ecasound::Multitrack::Track::by_index[$t->{n}]->set($_ => $t->{$_})
			} keys %track;
	} @tracks_data[0,1];

	splice @tracks_data, 0, 2;

	create_master_and_mix_tracks(); # their GUI only

	# create user tracks
	
	my $did_apply = 0;

	map{ 
		my %h = %$_; 
		#print "old n: $h{n}\n";
		#print "h: ", join " ", %h, $/;
		delete $h{n};
		#my @hh = %h; print "size: ", scalar @hh, $/;
		my $track = Audio::Ecasound::Multitrack::Track->new( %h ) ;
		my $n = $track->n;
		#print "new n: $n\n";
		$debug and print "restoring track: $n\n";
		$ui->track_gui($n); 
		
		for my $id (@{$ti[$n]->ops}){
			$did_apply++ 
				unless $id eq $ti[$n]->vol
					or $id eq $ti[$n]->pan;
			
			add_effect({
						chain => $cops{$id}->{chain},
						type => $cops{$id}->{type},
						cop_id => $id,
						parent_id => $cops{$id}->{belongs_to},
						});

		# TODO if parent has a parent, i am a parameter controller controlling
		# a parameter controller, and therefore need the -kx switch
		}
	} @tracks_data;
	#print "\n---\n", $tracker->dump;  
	#print "\n---\n", map{$_->dump} Audio::Ecasound::Multitrack::Track::all;# exit; 
	$did_apply and $ui->manifest;
	$debug and print join " ", 
		(map{ ref $_, $/ } @Audio::Ecasound::Multitrack::Track::by_index), $/;



	#my $toggle_jack = $widget_o[$#widget_o]; # JACK
	#convert_to_jack if $jack_on;
	#$ui->paint_button($toggle_jack, q(lightblue)) if $jack_on;
	$ui->refresh_oids();

	# restore Alsa mixer settings
	if ( $opts{a} ) {
		my $file = $file;
		$file =~ s/\.yml$//;
		print "restoring ALSA settings\n";
		print qx(alsactl -f $file.alsa restore);
	}

	# text mode marks 
		
	map{ 
		my %h = %$_; 
		my $mark = Audio::Ecasound::Multitrack::Mark->new( %h ) ;
	} @marks_data;
	$ui->restore_time_marks();

} 
sub create_master_and_mix_tracks { # GUI widgets
	$debug2 and print "&create_master_and_mix_tracks\n";


	my @rw_items = (
			[ 'command' => "MON",
				-command  => sub { 
						$tn{Master}->set(rw => "MON");
						refresh_track($master_track->n);
			}],
			[ 'command' => "OFF", 
				-command  => sub { 
						$tn{Master}->set(rw => "OFF");
						refresh_track($master_track->n);
			}],
		);

	$ui->track_gui( $master_track->n, @rw_items );

	$ui->track_gui( $mixdown_track->n); 

	$ui->group_gui('Tracker');
}


sub save_effects {
	$debug2 and print "&save_effects\n";
	my $file = shift;
	
	# restore muted volume levels
	#
	my %muted;
	
	map  {$copp{ $ti[$_]->vol }->[0] = $old_vol{$_} ;
		  $ui->paint_button($track_widget{$_}{mute}, $old_bg ) }
	grep { $old_vol{$_} }  # old vol level stored and muted
	all_chains();

	# we need the ops list for each track
	#
	# i dont see why, do we overwrite the effects section
	# in one of the init routines?
	# I will follow for now 12/6/07
	
	%state_c_ops = ();
	map{ 	$state_c_ops{$_} = $ti[$_]->ops } all_chains();

	# map {remove_op} @{ $ti[$_]->ops }

	store_vars(
		-file => $file, 
		-vars => \@effects_dynamic_vars,
		-class => 'Audio::Ecasound::Multitrack');

}

sub retrieve_effects {
	$debug2 and print "&retrieve_effects\n";
	my $file = shift;
	my %current_cops = %cops; # 
	my %current_copp = %copp; # 
	assign_vars($file, @effects_dynamic_vars);
	my %old_copp = %copp;  # 
	my %old_cops = %cops; 
	%cops = %current_cops;
	%copp = %current_copp; ## similar name!!


	#print "\%state_c_ops\n ", yaml_out( \%state_c_ops), "\n\n";
	#print "\%old_cops\n ", yaml_out( \%old_cops), "\n\n";
	#print "\%old_copp\n ", yaml_out( \%old_copp), "\n\n";
#	return;

	restore_time_marker_labels();

	# remove effects except vol and pan, in which case, update vals

	map{ 	
	
		$debug and print "found chain $_: ", join " ",
		@{ $ti[$_]->ops }, "\n";

		my $n = $_;
		map {	my $id = $_; 
				$debug and print "checking chain $n, id $id: ";
				
				if (	$ti[$n]->vol eq $id or
						$ti[$n]->pan eq $id  ){

					# do nothing
				$debug and print "is vol/pan\n";

				}
				else {
					
					$debug and print "is something else\n";
					remove_effect($id) ;
					remove_op($id)
			}

		} @{ $ti[$_]->ops }
	} all_chains();
			
	return;

	# restore ops list
	
	map{ $ti[$_]->set(ops => $state_c_ops{$_}) } all_chains();

	# restore ops->chain mapping
	
	%cops = %old_copp;

	# add the correct copp entry for each id except vol/pan
	map{ my $n = $_;
			map {	my $id = $_; 
				if (	$ti[$n]->vol eq $id or
						$ti[$n]->pan eq $id  ){

					$copp{$id}->[0] = $old_copp{$id}->[0];
				}
				else {  $copp{$id} = $old_copp{$id} }

			} @{ $ti[$_]->ops }
		} all_chains();

	# apply ops
	
	my $did_apply = 0;

	for my $n (all_chains() ) { 
		for my $id (@{$ti[$n]->ops}){
			$did_apply++ 
				unless $id eq $ti[$n]->vol
					or $id eq $ti[$n]->pan;

			
			add_effect({  
						chain => $cops{$id}->{chain},
						type => $cops{$id}->{type},
						cop_id => $id,
						parent_id => $cops{$id}->{belongs_to},
						});

		# TODO if parent has a parent, i am a parameter controller controlling
		# a parameter controller, and therefore need the -kx switch
		}
	}
	# $did_apply and print "########## applied\n\n";
	
	$ew->deiconify or $ew->iconify;

}

	

### end


# gui handling
#
use Carp;

sub add_effect_gui {
		$debug2 and print "&add_effect_gui\n";
		@_ = discard_object(@_);
		my %p 			= %{shift()};
		my $n 			= $p{chain};
		my $code 			= $p{type};
		my $parent_id = $p{parent_id};  
		my $id		= $p{cop_id};   # initiates restore
		my $parameter		= $p{parameter}; 
		my $i = $effect_i{$code};

		$debug and print yaml_out(\%p);

		$debug and print "cop_id: $id, parent_id: $parent_id\n";
		# $id is determined by cop_add, which will return the
		# existing cop_id if supplied

		# check display format, may be 'scale' 'field' or 'hidden'
		
		my $display_type = $cops{$id}->{display}; # individual setting
		defined $display_type or $display_type = $effects[$i]->{display}; # template
		$debug and print "display type: $display_type\n";

		return if $display_type eq q(hidden);

		my $frame ;
		if ( ! $parent_id ){ # independent effect
			$frame = $track_widget{$n}->{parents}->Frame->pack(
				-side => 'left', 
				-anchor => 'nw',)
		} else {                 # controller
			$frame = $track_widget{$n}->{children}->Frame->pack(
				-side => 'top', 
				-anchor => 'nw')
		}

		$effects_widget{$id} = $frame; 
		# we need a separate frame so title can be long

		# here add menu items for Add Controller, and Remove

		my $parentage = $effects[ $effect_i{ $cops{$parent_id}->{type}} ]
			->{name};
		$parentage and $parentage .=  " - ";
		$debug and print "parentage: $parentage\n";
		my $eff = $frame->Menubutton(
			-text => $parentage. $effects[$i]->{name}, -tearoff => 0,);

		$eff->AddItems([
			'command' => "Remove",
			-command => sub { remove_effect($id) }
		]);
		$eff->grid();
		my @labels;
		my @sliders;

		# make widgets

		for my $p (0..$effects[$i]->{count} - 1 ) {
		my @items;
		#$debug and print "p_first: $p_first, p_last: $p_last\n";
		for my $j ($e_bound{ctrl}{a}..$e_bound{ctrl}{z}) {   
			push @items, 				
				[ 'command' => $effects[$j]->{name},
					-command => sub { add_effect ({
							parent_id => $id,
							chain => $n,
							parameter  => $p,
							type => $effects[$j]->{code} } )  }
				];

		}
		push @labels, $frame->Menubutton(
				-text => $effects[$i]->{params}->[$p]->{name},
				-menuitems => [@items],
				-tearoff => 0,
		);
			$debug and print "parameter name: ",
				$effects[$i]->{params}->[$p]->{name},"\n";
			my $v =  # for argument vector 
			{	parent => \$frame,
				cop_id => $id, 
				p_num  => $p,
			};
			push @sliders,make_scale($v);
		}

		if (@sliders) {

			$sliders[0]->grid(@sliders[1..$#sliders]);
			 $labels[0]->grid(@labels[1..$#labels]);
		}
}


sub project_label_configure{ 
	@_ = discard_object(@_);
	$project_label->configure( @_ ) }

sub length_display{ 
	@_ = discard_object(@_);
	$setup_length->configure(@_)};

sub clock_config { 
	@_ = discard_object(@_);
	$clock->configure( @_ )}

sub manifest { $ew->deiconify() }

sub destroy_widgets {

	map{ $_->destroy } map{ $_->children } $effect_frame;
	#my @children = $group_frame->children;
	#map{ $_->destroy  } @children[1..$#children];
	my @children = $track_frame->children;
	# leave field labels (first row)
	map{ $_->destroy  } @children[10..$#children]; # fragile
	%mark_widget and map{ $_->destroy } values %mark_widget;
}

sub init_gui {

	$debug2 and print "&init_gui\n";

	@_ = discard_object(@_);

### 	Tk root window layout


	#my $mw = tkinit();
	$mw->optionAdd('*font', 'Helvetica 12');
	$mw->title("Ecasound/Nama"); 
	$mw->deiconify;

	### init effect window

	$ew = $mw->Toplevel;
	$ew->title("Effect Window");
	$ew->deiconify; 
	$ew->withdraw;

	$canvas = $ew->Scrolled('Canvas')->pack;
	$canvas->configure(
		scrollregion =>[2,2,10000,2000],
		-width => 900,
		-height => 600,	
		);
# 		scrollregion =>[2,2,10000,2000],
# 		-width => 1000,
# 		-height => 4000,	
	$effect_frame = $canvas->Frame;
	my $id = $canvas->createWindow(30,30, -window => $effect_frame,
											-anchor => 'nw');

	$project_label = $mw->Label->pack(-fill => 'both');
	$old_bg = $project_label->cget('-background');
	$time_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$mark_frame = $time_frame->Frame->pack(
		-side => 'bottom', 
		-fill => 'both');
	$fast_frame = $time_frame->Frame->pack(
		-side => 'bottom', 
		-fill => 'both');
	$transport_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$oid_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$clock_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	#$group_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$track_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
 	#$group_label = $group_frame->Menubutton(-text => "GROUP",
 #										-tearoff => 0,
 #										-width => 13)->pack(-side => 'left');
		
	$add_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$perl_eval_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$iam_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
	$load_frame = $mw->Frame->pack(-side => 'bottom', -fill => 'both');
#	my $blank = $mw->Label->pack(-side => 'left');



	$sn_label = $load_frame->Label(-text => "Project name: ")->pack(-side => 'left');
	$sn_text = $load_frame->Entry(-textvariable => \$project, -width => 25)->pack(-side => 'left');
	$sn_load = $load_frame->Button->pack(-side => 'left');;
	$sn_new = $load_frame->Button->pack(-side => 'left');;
	$sn_quit = $load_frame->Button->pack(-side => 'left');
	$sn_save = $load_frame->Button->pack(-side => 'left');
	$save_id = "";
	my $sn_save_text = $load_frame->Entry(
									-textvariable => \$save_id,
									-width => 15
									)->pack(-side => 'left');
	$sn_recall = $load_frame->Button->pack(-side => 'left');
	# $sn_dump = $load_frame->Button->pack(-side => 'left');

	$build_track_label = $add_frame->Label(
		-text => "          Name: ")->pack(-side => 'left');
	$build_track_text = $add_frame->Entry(-textvariable => \$track_name, -width => 12)->pack(-side => 'left');
	$build_track_rec_label = $add_frame->Label(-text => "Input channel:")->pack(-side => 'left');
	$build_track_rec_text = $add_frame->Entry(-textvariable => \$ch_r, -width => 2)->pack(-side => 'left');
	# $build_track_mon_label = $add_frame->Label(-text => "Mon CH")->pack(-side => 'left');
	# $build_track_mon_text = $add_frame->Entry(-textvariable => \$ch_m, -width => 2)->pack(-side => 'left');
	$build_track_add = $add_frame->Button->pack(-side => 'left');;

	$sn_load->configure(
		-text => 'Load',
		-command => sub{ load_project(
			name => remove_spaces $project_name),
			});
	$sn_new->configure( 
		-text => 'Create',
		-command => sub{ load_project(
							name => remove_spaces($project_name),
							create => 1)});
	$sn_save->configure(
		-text => 'Save settings',
		-command => #sub { print "save_id: $save_id\n" });
		 sub {save_state($save_id) });
	$sn_recall->configure(
		-text => 'Recall settings',
 		-command => sub {load_project (name => $project_name, 
 										settings => $save_id)},
				);
# 	$sn_dump->configure(
# 		-text => q(Dump state),
# 		-command => sub{ print &status_vars });
	$sn_quit->configure(-text => "Quit",
		 -command => sub { 
				return if transport_running();
				exit;
				 }
				);


	$build_track_add->configure( 
			-text => 'Add New Track',
			-command => sub { 
					return if $track_name =~ /^\s*$/;	
			add_track(remove_spaces($track_name)) }
	);

	my @labels = 
		qw(Track Name Version Status Input Volume Mute Unity Pan Center Effects);
	my @widgets;
	map{ push @widgets, $track_frame->Label(-text => $_)  } @labels;
	$widgets[0]->grid(@widgets[1..$#widgets]);

#  unified command processing by command_process 
	
	$iam_label = $iam_frame->Label(
	-text => "    Command: "
		)->pack(-side => 'left');;
	$iam_text = $iam_frame->Entry( 
		-textvariable => \$iam, -width => 45)
		->pack(-side => 'left');;
	$iam_execute = $iam_frame->Button(
			-text => 'Execute',
			-command => sub { print $iam; Audio::Ecasound::Multitrack::Text::command_process( $iam ) }
			
		)->pack(-side => 'left');;

			#join  " ",
			# grep{ $_ !~ add fxa afx } split /\s*;\s*/, $iam) 
		
}
sub transport_gui {
	@_ = discard_object(@_);
	$debug2 and print "&transport_gui\n";

	$transport_label = $transport_frame->Label(
		-text => 'TRANSPORT',
		-width => 12,
		)->pack(-side => 'left');;
	$transport_setup_and_connect  = $transport_frame->Button->pack(-side => 'left');;
	$transport_start = $transport_frame->Button->pack(-side => 'left');
	$transport_stop = $transport_frame->Button->pack(-side => 'left');
	#$transport_setup = $transport_frame->Button->pack(-side => 'left');;
	#$transport_connect = $transport_frame->Button->pack(-side => 'left');;
	#$transport_disconnect = $transport_frame->Button->pack(-side => 'left');;
	# $transport_new = $transport_frame->Button->pack(-side => 'left');;

	$transport_stop->configure(-text => "Stop",
	-command => sub { 
					stop_transport();
				}
		);
	$transport_start->configure(
		-text => "Start",
		-command => sub { 
		return if transport_running();
		if ( really_recording ) {
			project_label_configure(-background => 'lightpink') 
		}
		else {
			project_label_configure(-background => 'lightgreen') 
		}
		start_transport();
				});
	$transport_setup_and_connect->configure(
			-text => 'Arm',
			-command => sub {&generate_setup and &connect_transport}
						 );
# 	$transport_disconnect->configure(
# 			-text => 'Disconnect setup',
# 			-command => \&disconnect_transport,
# 						);
# 	$transport_new->configure(
# 			-text => 'New Engine',
# 			-command => \&new_engine,
# 						 );
}
sub time_gui {
	@_ = discard_object(@_);
	$debug2 and print "&time_gui\n";

	my $time_label = $clock_frame->Label(
		-text => 'TIME', 
		-width => 12);
	$clock = $clock_frame->Label(
		-text => '0:00', 
		-width => 8,
		-background => 'orange',
		);
	my $length_label = $clock_frame->Label(
		-text => 'LENGTH',
		-width => 10,
		);
	$setup_length = $clock_frame->Label(
	#	-width => 8,
		);

	for my $w ($time_label, $clock, $length_label, $setup_length) {
		$w->pack(-side => 'left');	
	}

	$mark_frame = $time_frame->Frame->pack(
		-side => 'bottom', 
		-fill => 'both');
	my $fast_frame = $time_frame->Frame->pack(
		-side => 'bottom', 
		-fill => 'both');
	# jump

	my $jump_label = $fast_frame->Label(-text => q(JUMP), -width => 12);
	my @pluses = (1, 5, 10, 30, 60);
	my @minuses = map{ - $_ } reverse @pluses;
	my @fw = map{ my $d = $_; $fast_frame->Button(
			-text => $d,
			-command => sub { jump($d) },
			)
		}  @pluses ;
	my @rew = map{ my $d = $_; $fast_frame->Button(
			-text => $d,
			-command => sub { jump($d) },
			)
		}  @minuses ;
	my $beg = $fast_frame->Button(
			-text => 'Beg',
			-command => \&to_start,
			);
	my $end = $fast_frame->Button(
			-text => 'End',
			-command => \&to_end,
			);

	$time_step = $fast_frame->Button( 
			-text => 'Sec',
			);
		for my $w($jump_label, @rew, $beg, $time_step, $end, @fw){
			$w->pack(-side => 'left')
		}

	$time_step->configure (-command => sub { &toggle_unit; &show_unit });

	# Marks
	
	my $mark_label = $mark_frame->Label(
		-text => q(MARK), 
		-width => 12,
		)->pack(-side => 'left');
		
	my $drop_mark = $mark_frame->Button(
		-text => 'Place',
		-background => $old_bg,
		-command => \&drop_mark,
		)->pack(-side => 'left');	
		
	$mark_remove = $mark_frame->Button(
		-text => 'Remove',
		-command => \&arm_mark_toggle,
	)->pack(-side => 'left');	

}

sub oid_gui {
	$debug2 and print "&oid_gui\n";
	@_ = discard_object(@_);
	my $outputs = $oid_frame->Label(-text => 'OUTPUTS', -width => 12);
	my @oid_name;
	for my $rule ( Audio::Ecasound::Multitrack::Rule::all_rules ){
		my $name = $rule->name;
		my $status = $rule->status;
		# print "gui oid name: $name status: $status\n";
		next if $name =~ m/setup|mix_|mixer|rec_file|multi/i;
		push @oid_name, $name;
		
		my $oid_button = $oid_frame->Button( 
			-text => ucfirst $name,
			-background => 
				$status ?  'AntiqueWhite' : $old_bg,
			-activebackground => 
				$status ? 'AntiqueWhite' : $old_bg
		);
		$oid_button->configure(
			-command => sub { 
				$rule->set(status => ! $rule->status);
				$oid_button->configure( -background => 
					$rule->status ?  'AntiqueWhite' : $old_bg ,
			-activebackground => 
					$rule->status ? 'AntiqueWhite' : $old_bg
					
					);
			});
		push @widget_o, $oid_button;
	}
	my $toggle_jack = $oid_frame->Button;
	
	$toggle_jack->configure(
		-text => q(Jack ON/OFF),
		-command => sub {
			my $color = $toggle_jack->cget( -background );
				if ($color eq q(lightblue) ){

					# jack is on, turn it off
				
					convert_to_alsa();
					paint_button($toggle_jack, $old_bg);
					$jack_on = 0;
				}
				else {

					convert_to_jack();
					paint_button($toggle_jack, q(lightblue));
					$jack_on = 1;
				}
			}
		);
	push @widget_o, $toggle_jack; # since no one else uses this array
				
		
	map { $_ -> pack(-side => 'left') } ($outputs, @widget_o);
	
}
sub paint_button {
	@_ = discard_object(@_);
	my ($button, $color) = @_;
	$button->configure(-background => $color,
						-activebackground => $color);
}
sub flash_ready {
	my $color;
		if ( user_rec_tracks()  ){
			$color = 'lightpink'; # live recording
		} elsif ( &really_recording ){  # mixdown only 
			$color = 'yellow';
		} elsif ( user_mon_tracks() ){  
			$color = 'lightgreen'; }; # just playback

	$debug and print "flash color: $color\n";
	length_display(-background => $color);
	$clock_id->cancel if (ref $clock_id) =~ /Tk/;
	$clock_id = $clock->after(3000, 
		sub{ length_display(-background => $old_bg) }
	);
}
sub group_gui {  
	@_ = discard_object(@_);
	my $group = $tracker; 
	my $label;
	my $dummy = $track_frame->Label(-text => ' '); 
	$label = 	$track_frame->Label(-text => "Group" );
	$group_version = $track_frame->Menubutton(-tearoff => 0);
	$group_rw = $track_frame->Menubutton( -text    => $group->rw,
										  -tearoff => 0);


		
		$group_rw->AddItems([
			'command' => 'REC',
			-background => $old_bg,
			-command => sub { 
				$group->set(rw => 'REC');
				$group_rw->configure(-text => 'REC');
				refresh();
				generate_setup() and connect_transport()
				}
			],[
			'command' => 'MON',
			-background => $old_bg,
			-command => sub { 
				$group->set(rw => 'MON');
				$group_rw->configure(-text => 'MON');
				refresh();
				generate_setup() and connect_transport()
				}
			],[
			'command' => 'OFF',
			-background => $old_bg,
			-command => sub { 
				$group->set(rw => 'OFF');
				$group_rw->configure(-text => 'OFF');
				refresh();
				generate_setup() and connect_transport()
				}
			]);
			$dummy->grid($label, $group_version, $group_rw);
			$ui->global_version_buttons;

}
sub global_version_buttons {
	local $debug = 0;
	my $version = $group_version;
	$version and map { $_->destroy } $version->children;
		
	$debug and print "making global version buttons range:",
		join ' ',1..$ti[-1]->group_last, " \n";

			$version->radiobutton( 

				-label => (''),
				-value => 0,
				-command => sub { 
					$tracker->set(version => 0); 
					$version->configure(-text => " ");
					generate_setup() and connect_transport();
					refresh();
					}

 					);

 	for my $v (1..$ti[-1]->group_last) { 

	# the highest version number of all tracks in the
	# $tracker group
	
	my @user_track_indices = grep { $_ > 2 } map {$_->n} Audio::Ecasound::Multitrack::Track::all;
	
		next unless grep{  grep{ $v == $_ } @{ $ti[$_]->versions } }
			@user_track_indices;
		

			$version->radiobutton( 

				-label => ($v ? $v : ''),
				-value => $v,
				-command => sub { 
					$tracker->set(version => $v); 
					$version->configure(-text => $v);
					generate_setup() and connect_transport();
					refresh();
					}

 					);
 	}
}
sub track_gui { 
	$debug2 and print "&track_gui\n";
	@_ = discard_object(@_);
	my $n = shift;
	$debug and print "found index: $n\n";
	my @rw_items = @_ ? @_ : (
			[ 'command' => "REC",
				-foreground => 'red',
				-command  => sub { 
					$ti[$n]->set(rw => "REC");
					
					refresh_track($n);
					refresh_group();
			}],
			[ 'command' => "MON",
				-command  => sub { 
					$ti[$n]->set(rw => "MON");
					refresh_track($n);
					refresh_group();
			}],
			[ 'command' => "OFF", 
				-command  => sub { 
					$ti[$n]->set(rw => "OFF");
					refresh_track($n);
					refresh_group();
			}],
		);
	my ($number, $name, $version, $rw, $ch_r, $ch_m, $vol, $mute, $solo, $unity, $pan, $center);
	$number = $track_frame->Label(-text => $n,
									-justify => 'left');
	my $stub = " ";
	$stub .= $ti[$n]->active;
	$name = $track_frame->Label(
			-text => $ti[$n]->name,
			-justify => 'left');
	$version = $track_frame->Menubutton( 
					-text => $stub,
					-tearoff => 0);
	my @versions = '';
	#push @versions, @{$ti[$n]->versions} if @{$ti[$n]->versions};
	my $ref = ref $ti[$n]->versions ;
		$ref =~ /ARRAY/ and 
		push (@versions, @{$ti[$n]->versions}) or
		croak "chain $n, found unexpectedly $ref\n";;
	for my $v (@versions) {
					$version->radiobutton(
						-label => $v,
						# -value => $v,
						-command => 
		sub { 
			$ti[$n]->set( active => $v );
			return if $ti[$n]->rec_status eq "REC";
			$version->configure( -text=> $ti[$n]->current_version ) 
			}
					);
	}

	# skip the rest of the widgets for the mixdown track
	
# if ( $n != 2 ){


	$ch_r = $track_frame->Menubutton(
					-tearoff => 0,
				);
	my @range;
	push @range, "";
	push @range, 1..$tk_input_channels if $n > 2;
	
	for my $v (@range) {
		$ch_r->radiobutton(
			-label => $v,
			-value => $v,
			-command => sub { 
			#	$ti[$n]->set(rw => 'REC');
				$ti[$n]->set(ch_r  => $v);
				refresh_track($n) }
			)
	}
	$ch_m = $track_frame->Menubutton(
					-tearoff => 0,
				);
				for my $v ("",1..10) {
					$ch_m->radiobutton(
						-label => $v,
						-value => $v,
						-command => sub { 
			#				$ti[$n]->set(rw  => "MON");
							$ti[$n]->set(ch_m  => $v);
							refresh_track($n) }
				 		)
				}
	$rw = $track_frame->Menubutton(
		-text => $ti[$n]->rw,
		-tearoff => 0,
	);
	map{$rw->AddItems($_)} @rw_items; 

 
	# Volume

	my $p_num = 0; # needed when using parameter controllers
	my $vol_id = $ti[$n]->vol;

	local $debug = 0;


	$debug and print "vol cop_id: $vol_id\n";
	my %p = ( 	parent => \$track_frame,
			chain  => $n,
			type => 'ea',
			cop_id => $vol_id,
			p_num		=> $p_num,
			length => 300, 
			);


	 $debug and do {my %q = %p; delete $q{parent}; print
	 "=============\n%p\n",yaml_out(\%q)};

	$vol = make_scale ( \%p );
	# Mute

	$mute = $track_frame->Button(
	  		-command => sub { 
				if ($copp{$vol_id}->[0]) {  # non-zero volume
					$old_vol{$n}=$copp{$vol_id}->[0];
					$copp{$vol_id}->[0] = 0;
					effect_update($p{chain}, $p{cop_id}, $p{p_num}, 0);
					$mute->configure(-background => 'brown');
					$mute->configure(-activebackground => 'brown');
				}
				else {
					$copp{$vol_id}->[0] = $old_vol{$n};
					effect_update($p{chain}, $p{cop_id}, $p{p_num}, 
						$old_vol{$n});
					$old_vol{$n} = 0;
					$mute->configure(-background => $old_bg);
					$mute->configure(-activebackground => $old_bg);
				}
			}	
	  );


	# Unity

	$unity = $track_frame->Button(
	  		-command => sub { 
				$copp{$vol_id}->[0] = 100;
	 			effect_update($p{chain}, $p{cop_id}, $p{p_num}, 100);
			}
	  );

	  
	# Pan
	
	my $pan_id = $ti[$n]->pan;
	
	$debug and print "pan cop_id: $pan_id\n";
	$p_num = 0;           # first parameter
	my %q = ( 	parent => \$track_frame,
			chain  => $n,
			type => 'epp',
			cop_id => $pan_id,
			p_num		=> $p_num,
			);
	# $debug and do { my %q = %p; delete $q{parent}; print "x=============\n%p\n",yaml_out(\%q) };
	$pan = make_scale ( \%q );

	# Center

	$center = $track_frame->Button(
	  	-command => sub { 
			$copp{$pan_id}->[0] = 50;
			effect_update($q{chain}, $q{cop_id}, $q{p_num}, 50);
		}
	  );
	
	my $effects = $effect_frame->Frame->pack(-fill => 'both');;

	# effects, held by track_widget->n->effects is the frame for
	# all effects of the track

	@{ $track_widget{$n} }{qw(name version rw ch_r ch_m mute effects)} 
		= ($name,  $version, $rw, $ch_r, $ch_m, $mute, \$effects);#a ref to the object
	#$debug and print "=============\n\%track_widget\n",yaml_out(\%track_widget);
	my $independent_effects_frame 
		= ${ $track_widget{$n}->{effects} }->Frame->pack(-fill => 'x');


	my $controllers_frame 
		= ${ $track_widget{$n}->{effects} }->Frame->pack(-fill => 'x');
	
	# parents are the independent effects
	# children are controllers for various paramters

	$track_widget{$n}->{parents} = $independent_effects_frame;

	$track_widget{$n}->{children} = $controllers_frame;
	
	$independent_effects_frame
		->Label(-text => uc $ti[$n]->name )->pack(-side => 'left');

	#$debug and print( "Number: $n\n"),MainLoop if $n == 2;
	my @tags = qw( EF P1 P2 L1 L2 L3 L4 );
	my @starts =   ( $e_bound{tkeca}{a}, 
					 $e_bound{preset}{a}, 
					 $e_bound{preset}{b}, 
					 $e_bound{ladspa}{a}, 
					 $e_bound{ladspa}{b}, 
					 $e_bound{ladspa}{c}, 
					 $e_bound{ladspa}{d}, 
					);
	my @ends   =   ( $e_bound{tkeca}{z}, 
					 $e_bound{preset}{b}, 
					 $e_bound{preset}{z}, 
					 $e_bound{ladspa}{b}-1, 
					 $e_bound{ladspa}{c}-1, 
					 $e_bound{ladspa}{d}-1, 
					 $e_bound{ladspa}{z}, 
					);
	my @add_effect;

	map{push @add_effect, effect_button($n, shift @tags, shift @starts, shift @ends)} 1..@tags;
	
	#$name->grid($version, $rw, $ch_r, $ch_m, $vol, $mute, $unity, $pan, $center, @add_effect);

	$number->grid($name, $version, $rw, $ch_r, $vol, $mute, $unity, $pan, $center, @add_effect);
	refresh_track($n);

}

sub update_version_button {
	@_ = discard_object(@_);
	my ($n, $v) = @_;
	carp ("no version provided \n") if ! $v;
	my $w = $track_widget{$n}->{version};
					$w->radiobutton(
						-label => $v,
						-value => $v,
						-command => 
		sub { $track_widget{$n}->{version}->configure(-text=>$v) 
				unless $ti[$n]->rec_status eq "REC" }
					);
}

sub effect_button {
	local $debug = 0;	
	$debug2 and print "&effect_button\n";
	my ($n, $label, $start, $end) = @_;
	$debug and print "chain $n label $label start $start end $end\n";
	my @items;
	my $widget;
	my @indices = ($start..$end);
	if ($start >= $e_bound{ladspa}{a} and $start <= $e_bound{ladspa}{z}){
		@indices = ();
		@indices = @ladspa_sorted[$start..$end];
		$debug and print "length sorted indices list: ".scalar @indices. "\n";
	$debug and print "Indices: @indices\n";
	}
		
		for my $j (@indices) { 
		push @items, 				
			[ 'command' => "$effects[$j]->{count} $effects[$j]->{name}" ,
				-command  => sub { 
					 add_effect( {chain => $n, type => $effects[$j]->{code} } ); 
					$ew->deiconify; # display effects window
					} 
			];
	}
	$widget = $track_frame->Menubutton(
		-text => $label,
		-tearoff =>0,
		-menuitems => [@items],
	);
	$widget;
}

sub make_scale {
	
	$debug2 and print "&make_scale\n";
	my $ref = shift;
	my %p = %{$ref};
# 	%p contains following:
# 	cop_id   => operator id, to access dynamic effect params in %copp
# 	parent => parent widget, i.e. the frame
# 	p_num      => parameter number, starting at 0
# 	length       => length widget # optional 
	my $id = $p{cop_id};
	my $n = $cops{$id}->{chain};
	my $code = $cops{$id}->{type};
	my $p  = $p{p_num};
	my $i  = $effect_i{$code};

	$debug and print "id: $id code: $code\n";
	

	# check display format, may be text-field or hidden,

	$debug and  print "i: $i code: $effects[$i]->{code} display: $effects[$i]->{display}\n";
	my $display_type = $cops{$id}->{display};
	defined $display_type or $display_type = $effects[$i]->{display};
	$debug and print "display type: $display_type\n";
	return if $display_type eq q(hidden);


	$debug and print "to: ", $effects[$i]->{params}->[$p]->{end}, "\n";
	$debug and print "p: $p code: $code\n";

	# set display type to individually specified value if it exists
	# otherwise to the default for the controller class


	
	if 	($display_type eq q(scale) ) { 

		# return scale type controller widgets
		my $frame = ${ $p{parent} }->Frame;
			

		#return ${ $p{parent} }->Scale(
		
		my $log_display;
		
		my $controller = $frame->Scale(
			-variable => \$copp{$id}->[$p],
			-orient => 'horizontal',
			-from   =>   $effects[$i]->{params}->[$p]->{begin},
			-to   =>     $effects[$i]->{params}->[$p]->{end},
			-resolution => ($effects[$i]->{params}->[$p]->{resolution} 
				?  $effects[$i]->{params}->[$p]->{resolution}
				: abs($effects[$i]->{params}->[$p]->{end} - 
					$effects[$i]->{params}->[$p]->{begin} ) > 30 
						? 1 
						: abs($effects[$i]->{params}->[$p]->{end} - 
							$effects[$i]->{params}->[$p]->{begin} ) / 100),
		  -width => 12,
		  -length => $p{length} ? $p{length} : 100,
		  -command => sub { effect_update($n, $id, $p, $copp{$id}->[$p]) }
		  );

		# auxiliary field for logarithmic display
		if ($effects[$i]->{params}->[$p]->{hint} =~ /logarithm/ )
		#	or $code eq 'ea') 
		
			{
			my $log_display = $frame->Label(
				-text => exp $effects[$i]->{params}->[$p]->{default},
				-width => 5,
				);
			$controller->configure(
		  		-command => sub { 
					effect_update($n, $id, $p, exp $copp{$id}->[$p]);
					$log_display->configure(
						-text => 
						$effects[$i]->{params}->[$p]->{name} =~ /hz/i
							? int exp $copp{$id}->[$p]
							: dn(exp $copp{$id}->[$p], 1)
						);
					}
				);
		$log_display->grid($controller);
		}
		else { $controller->grid; }

		return $frame;

	}	

	elsif ($display_type eq q(field) ){ 

	 	# then return field type controller widget

		return ${ $p{parent} }->Entry(
			-textvariable =>\$copp{$id}->[$p],
			-width => 6,
	#		-command => sub { effect_update($n, $id, $p, $copp{$id}->[$p]) },
			# doesn't work with Entry widget
			);	

	}
	else { croak "missing or unexpected display type: $display_type" }

}
sub arm_mark_toggle { 
	if ($markers_armed) {
		$markers_armed = 0;
		$mark_remove->configure( -background => $old_bg);
	}
	else{
		$markers_armed = 1;
		$mark_remove->configure( -background => 'yellow');
	}
}
sub marker {
	@_ = discard_object( @_); # UI
	my $mark = shift; # Mark
	#print "mark is ", ref $mark, $/;
	my $pos = $mark->time;
	#print $pos, " ", int $pos, $/;
		$mark_widget{$pos} = $mark_frame->Button( 
			-text => (join " ",  colonize( int $pos ), $mark->name),
			-background => $old_bg,
			-command => sub { mark($mark) },
		)->pack(-side => 'left');
}

sub restore_time_marks {
	@_ = discard_object( @_);
# 	map {$_->dumpp} Audio::Ecasound::Multitrack::Mark::all(); 
#	Audio::Ecasound::Multitrack::Mark::all() and 
	map{ $ui->marker($_) } Audio::Ecasound::Multitrack::Mark::all() ; 
	$time_step->configure( -text => $unit == 1 ? q(Sec) : q(Min) )
}
sub destroy_marker {
	@_ = discard_object( @_);
	my $pos = shift;
	$mark_widget{$pos}->destroy; 
}
sub colonize { # convert seconds to minutes:seconds 
	my $sec = shift;
	my $hours = int ($sec / 3600);
	$sec = $sec % 3600;
	my $min = int ($sec / 60);
	$sec = $sec % 60;
	$sec = "0$sec" if $sec < 10;
	$min = "0$min" if $min < 10 and $hours;
	($hours ? "$hours:" : "") . qq($min:$sec);
}
sub update_clock { 
	$ui->clock_config(-text => colonize(eval_iam('cs-get-position')));
}

sub start_heartbeat { Audio::Ecasound::Multitrack::start_heartbeat() }


### end


## refresh functions

sub set_widget_color {
	my ($widget, $status) = @_;
	my %rw_foreground = (REC  => 'Black', 
					MON => 'Black',
					OFF => 'Black');
	my %rw_background =  (REC  => 'LightPink', 
					MON => 'AntiqueWhite',
					OFF => $old_bg);

	$widget->configure( -background => $rw_background{$status} );
}


	
sub refresh_group { # tracker group 
	$debug2 and print "&refresh_group\n";
	
	
		my $status;
		if ( 	grep{ $_->rec_status eq 'REC'} 
				map{ $tn{$_} }
				$tracker->tracks ){

			$status = 'REC'

		}elsif(	grep{ $_->rec_status eq 'MON'} 
				map{ $tn{$_} }
				$tracker->tracks ){

			$status = 'MON'

		}else{ 
		
			$status = 'OFF' }

$debug and print "group status: $status\n";

	set_widget_color($group_rw, $status); 



	croak "some crazy status |$status|\n" if $status !~ m/rec|mon|off/i;
		#$debug and print "attempting to set $status color: ", $take_color{$status},"\n";

	set_widget_color( $group_rw, $status) if $group_rw;
}
sub refresh_track {
	
	@_ = discard_object(@_);
	my $n = shift;
	$debug2 and print "&refresh_track\n";
	
	my $rec_status = $ti[$n]->rec_status;
	$debug and print "track: $n rec_status: $rec_status\n";

	#	return unless $track_widget{$n}; # hidden track
		$track_widget{$n}->{rw}->configure(-text => $rec_status);
		 $track_widget{$n}->{ch_r}->configure( -text =>
		 $ti[$n]->ch_r || 1);
		 #$track_widget{$n}->{ch_m}->configure( -text => $ti[$n]->ch_m);
		$track_widget{$n}->{version}->configure(-text => $ti[$n]->current_version);
	
	if ($rec_status eq "REC") {

		$track_widget{$n}->{name}->configure(-background => 'lightpink');
		$track_widget{$n}->{name}->configure(-foreground => 'Black');
		$track_widget{$n}->{ch_r}->configure(-background => 'LightPink');
		$track_widget{$n}->{ch_r}->configure(-foreground => 'Black');
		$track_widget{$n}->{rw}->configure(-background => 'LightPink');
		$track_widget{$n}->{rw}->configure(-foreground => 'Black');
		#$track_widget{$n}->{ch_m}->configure( -background => $old_bg);
		#$track_widget{$n}->{ch_m}->configure( -foreground => 'DarkGray');

	}
	elsif ( $rec_status eq "MON" ) {

		 $track_widget{$n}->{name}->configure(-background => 'AntiqueWhite');
		 $track_widget{$n}->{name}->configure(-foreground => 'Black');
		 $track_widget{$n}->{ch_r}->configure( -background => $old_bg);
		 $track_widget{$n}->{ch_r}->configure( -foreground => $old_bg);
		# $track_widget{$n}->{ch_m}->configure( -background => 'AntiqueWhite');
		# $track_widget{$n}->{ch_m}->configure( -foreground => 'Black');
		$track_widget{$n}->{rw}->configure(-background => 'AntiqueWhite');
		$track_widget{$n}->{rw}->configure(-foreground => 'Black');

		}
	elsif ( $rec_status eq "OFF" ) {
		 $track_widget{$n}->{name}->configure(-background => $old_bg);
		 $track_widget{$n}->{ch_r}->configure( -background => $old_bg); 
		 $track_widget{$n}->{ch_r}->configure( -foreground => $old_bg);
		 #$track_widget{$n}->{ch_m}->configure( -background => $old_bg); 
		# $track_widget{$n}->{ch_m}->configure( -foreground => 'Gray');
		$track_widget{$n}->{rw}->configure(-background => $old_bg);
		$track_widget{$n}->{rw}->configure(-foreground => 'Black');
		}  
		else { carp "\$rec_status contains something unknown: $rec_status";}
}
sub refresh {  
	remove_small_wavs();
 	$ui->refresh_group(); 
	map{ $ui->refresh_track($_) } map{$_->n} Audio::Ecasound::Multitrack::Track::all();
}
sub refresh_oids{ # OUTPUT buttons
	map{ $widget_o{$_}->configure( # uses hash
			-background => 
				$oid_status{$_} ?  'AntiqueWhite' : $old_bg,
			-activebackground => 
				$oid_status{$_} ? 'AntiqueWhite' : $old_bg
			) } keys %widget_o;
}

### end


## The following code loads the object core of the system 
## and initiates the chain templates (rules)

use Audio::Ecasound::Multitrack::Track;   

package Audio::Ecasound::Multitrack::Graphical;  ## gui routines
our @ISA = 'Audio::Ecasound::Multitrack';
#use Tk;
#use Audio::Ecasound::Multitrack::Assign qw(:all);

## The following methods belong to the Graphical interface class

sub hello {"make a window";}
sub new { my $class = shift; return bless {@_}, $class }
sub loop {
    package Audio::Ecasound::Multitrack;
    #MainLoop;
    my $term = new Term::ReadLine 'Nama';
	$term->tkRunning(1);
    my $prompt = "Enter command: ";
    $OUT = $term->OUT || \*STDOUT;
	while (1) {
    my ($user_input) = $term->readline($prompt) ;
	next if $user_input =~ /^\s*$/;
     $term->addhistory($user_input) ;
	Audio::Ecasound::Multitrack::Text::command_process( $user_input );
	}
#   Term::Shell version
#
# 	my $shell = Audio::Ecasound::Multitrack::Text::OuterShell->new;
# 	my $term = $shell->term();
# 	$term->tkRunning(1);
# 	$shell->cmdloop;
}

## The following methods belong to the Text interface class

package Audio::Ecasound::Multitrack::Text;
our @ISA = 'Audio::Ecasound::Multitrack';
use Carp;
sub hello {"hello world!";}

## no-op graphic methods 

# those that take parameters will break!!!
# because object and procedural access get
# different parameter lists ($self being included);

sub start_heartbeat {}
sub start_clock {}
sub init_gui {}
sub transport_gui {}
sub group_gui {}
sub track_gui {}
sub time_gui {}
sub refresh {}
sub refresh_group {}
sub refresh_track {}
sub flash_ready {}
sub update_master_version_button {}
sub update_version_button {}
sub paint_button {}
sub refresh_oids {}
sub project_label_configure{}
sub length_display{}
sub clock_display {}
sub clock_config {}
sub manifest {}
sub global_version_buttons {}
sub destroy_widgets {}
sub destroy_marker {}
sub restore_time_marks {}
sub show_unit {};
sub add_effect_gui {};
sub remove_effect_gui {};
sub marker {};
## Some of these, may be overwritten
## by definitions that follow

use Carp;
sub new { my $class = shift; return bless { @_ }, $class; }

sub show_versions {
 	print "All versions: ", join " ", @{$Audio::Ecasound::Multitrack::this_track->versions}, $/;
}

sub show_effects {
 	map { 
 		my $op_id = $_;
 		 my $i = $Audio::Ecasound::Multitrack::effect_i{ $Audio::Ecasound::Multitrack::cops{ $op_id }->{type} };
 		 print $op_id, ": " , $Audio::Ecasound::Multitrack::effects[ $i ]->{name},  " ";
 		 my @pnames =@{$Audio::Ecasound::Multitrack::effects[ $i ]->{params}};
			map{ print join " ", 
			 	$pnames[$_]->{name}, 
				$Audio::Ecasound::Multitrack::copp{$op_id}->[$_],'' 
		 	} (0..scalar @pnames - 1);
		 print $/;
 
 	 } @{ $Audio::Ecasound::Multitrack::this_track->ops };
}
sub show_modifiers {
	print "Modifiers: ",$Audio::Ecasound::Multitrack::this_track->modifiers, $/;
}
sub loop {
    package Audio::Ecasound::Multitrack;
    #load_project(name => $project_name, create => $opts{c}) if $project_name;
    my $term = new Term::ReadLine 'Nama';
	
# 	No TK events in text-only mode

	# $mw->iconify;         
	# $term->tkRunning(1);
	
    my $prompt = "Enter command: ";
    $OUT = $term->OUT || \*STDOUT;
	while (1) {
    my ($user_input) = $term->readline($prompt) ;
	next if $user_input =~ /^\s*$/;
	#print "previous: '$previous_text_command' current: '$user_input'\n";
    $term->addhistory($user_input) 
	 	unless $user_input eq $previous_text_command;
 	$previous_text_command = $user_input;
	Audio::Ecasound::Multitrack::Text::command_process( $user_input );
	#print "here we are\n";
 #    use Audio::Ecasound::Multitrack::Text::OuterShell; # not needed, class is present in this file
#      my $shell = Audio::Ecasound::Multitrack::Text::OuterShell->new;

          # $shell->cmdloop;
	}
}

    
sub command_process {

package Audio::Ecasound::Multitrack;
        my ($user_input) = shift;
        return if $user_input =~ /^\s*$/;
        $debug and print "user input: $user_input\n";
		my ($cmd, $predicate) = ($user_input =~ /([\S]+)(.*)/);
		if ($cmd eq 'eval') {
                $debug and print "Evaluating perl code\n";
                print eval $predicate;
                print "\n";
                $@ and print "Perl command failed: $@\n";
		}
		elsif ( $cmd eq '!' ) {
                $debug and print "Evaluating shell commands!\n";
                system $predicate;
                print "\n";
		} else {


        my @user_input = split /\s*;\s*/, $user_input;
        map {
            my $user_input = $_;
            my ($cmd, $predicate) = ($user_input =~ /([\S]+)(.*)/);
            $debug and print "cmd: $cmd \npredicate: $predicate\n";
            if ($cmd eq 'eval') {
                $debug and print "Evaluating perl code\n";
                print eval $predicate;
                print "\n";
                $@ and print "Perl command failed: $@\n";
            } elsif ($cmd eq '!') {
                $debug and print "Evaluating shell commands!\n";
                system $predicate;
                print "\n";
            } elsif ($tn{$cmd}) { 
                $debug and print qq(Selecting track "$cmd"\n);
                $this_track = $tn{$cmd};
                $predicate !~ /^\s*$/ and $Audio::Ecasound::Multitrack::parser->command($predicate);
            } elsif ($cmd =~ /^\d+$/ and $ti[$cmd]) { 
                $debug and print qq(Selecting track ), $ti[$cmd]->name, $/;
                $this_track = $ti[$cmd];
                $predicate !~ /^\s*$/ and $Audio::Ecasound::Multitrack::parser->command($predicate);
            } elsif ($iam_cmd{$cmd}){
                $debug and print "Found Iam command\n";
                print Audio::Ecasound::Multitrack::eval_iam($user_input), $/ ;
            } else {
                $debug and print "Passing to parser\n", 
                $_, $/;
                #print 1, ref $parser, $/;
                #print 2, ref $Audio::Ecasound::Multitrack::parser, $/;
                # both print
                $parser->command($_) 
            }    
        } @user_input;
		}
        $ui->refresh; # in case we have a graphic environment
}
package Audio::Ecasound::Multitrack::Text;
sub show_tracks {
    no warnings;
    my @tracks = @_;
    map {     push @Audio::Ecasound::Multitrack::format_fields,  
            $_->n,
            $_->name,
            $_->rw,
            $_->rec_status,
            $_->rec_status eq 'REC' ? $_->ch_r : '',
            $_->current_version || '',
            #(join " ", @{$_->versions}),

        } grep{ ! $_-> hide} @tracks;
        
    write; # using format at end of file UI.pm
    $- = 0; # $FORMAT_LINES_LEFT # force header on next output
    1;
    use warnings;
    no warnings q(uninitialized);
}

format STDOUT_TOP =
Track  Name        Setting  Status  Input  Version 
==================================================
.
format STDOUT =
@>>    @<<<<<<<<<    @<<<    @<<<    @>>     @>>>   ~~
splice @Audio::Ecasound::Multitrack::format_fields, 0, 6
.

sub helpline {
	my $cmd = shift;
	my $text = "Command: $cmd\n";
	$text .=  "Shortcuts: $commands{$cmd}->{short}\n"
			if $commands{$cmd}->{short};	
	$text .=  $commands{$cmd}->{what}. $/;
	$text .=  "parameters: ". $commands{$cmd}->{parameters} . $/
			if $commands{$cmd}->{parameters};	
	$text .=  "example: ". eval( qq("$commands{$cmd}->{example}") ) . $/  
			if $commands{$cmd}->{example};
	print( $/, ucfirst $text, $/);
	
}
sub helptopic {
	my $index = shift;
	$index =~ /^\d+$/ and $index = $help_topic[$index];
	print "\n-- ", ucfirst $index, " --\n\n";
	print $help_topic{$index};
	print $/;
}

sub help { 
	my $name = shift;
	chomp $name;
	#print "seeking help for argument: $name\n";
	$help_topic{$name} and helptopic($name), return;
	$name == 10 and (map{ helptopic $_ } @help_topic), return;
	$name =~ /^\d+$/ and helptopic($name), return;

	$commands{$name} and helpline($name), return;
	my %helped = (); 
	map{  my $cmd = $_ ;
		helpline($cmd) and $helped{$cmd}++ if $cmd =~ /$name/;
		  # print ("commands short: ", $commands{$cmd}->{short}, $/),
	      helpline($cmd) 
		  	if grep { /$name/ } split " ", $commands{$cmd}->{short} 
				and ! $helped{$cmd};
	} keys %commands;
	# e.g. help tap_reverb
	if ( $effects_ladspa{"el:$name"}) {
	print "$name is the code for the following LADSPA effect:\n";
	#print yaml_out( $effects_ladspa{"el:$name"});
    print qx(analyseplugin $name);
	}
	
}


=comment
# prepare help and autocomplete

package Audio::Ecasound::Multitrack::Text::OuterShell;
use base qw(Term::Shell); 
#create_help_subs();
sub catch_run { # 
  my ($o, $cmd, @args) = @_;
  my $original_command_line = join " ", $cmd, @args;
  #print "foudn $0 $original_command_line\n";
  Audio::Ecasound::Multitrack::Text::command_process( $original_command_line );
}
sub catch_help {
  my ($o, $cmd, @args) = @_;
  $debug and print "cmd: $cmd\n";
  #my $main_name = 
  #
  print grep{ $_ eq $cmd } join " ", 
  my $main_name;
  CMD: for my $k ( keys %commands ){
      for my $alias ( $k, split " ",$commands{$k}{short} ){
        if ($cmd eq $alias){
            $main_name = $k;
            last CMD;
        }
    }
  }
  $debug and print "main_name: $main_name\n";
            
    my $txt = $o->help($main_name, @_);
    if ($o->{command}{help}{found}) {
        $o->page("$txt\n")
    }
}


#my $print "catched help @_"}
sub prompt_str { 'Enter command: ' }
sub run_help {
    my $o = shift;
    my $cmd = shift;
    if ($cmd) {
    my $txt = $o->help($cmd, @_);
    if ($o->{command}{help}{found}) {
        $o->page("$txt\n")
    }
    else {
        my @c = sort $o->possible_actions($cmd, 'help');
        if (@c and $o->{API}{match_uniq}) {
        local $" = "\n\t";
        print <<END;
Ambiguous help topic '$cmd': possible help topics:
    @c
END
        }
        else {
        print <<END;
Unknown help topic '$cmd'; type 'help' for a list of help topics.
END
        }
    }
    }
    else {
    print "Type 'help command' for more detailed help on a command.\n";
my $help_screen = <<HELP;
Help Screen Goes here
HELP
    $o->page($help_screen);
    }
}


sub create_help_subs {
    $debug2 and print "create_help_subs\n";
    %commands = %{ Audio::Ecasound::Multitrack::yaml_in( $Audio::Ecasound::Multitrack::commands_yml) };

    $debug and print Audio::Ecasound::Multitrack::yaml_out \%commands;
    
    map{ print $_, $/} grep{ $_ !~ /mark/ and $_ !~ /effect/ } keys %commands;
    
    map{ 
            my $run_code = qq!sub run_$_ { splice \@_,1,0,  q($_); catch_run( \@_) }; !;
            $debug and print "evalcode: $run_code\n";
            eval $run_code;
            $debug and $@ and print "create_sub eval error: $@\n";
            my $help_code = qq!sub help_$_ { q($commands{$_}{what}) };!;
            $debug and print "evalcode: $help_code\n";
            eval $help_code;
            $debug and $@ and print "create_sub eval error: $@\n";
            my $smry_text = 
            $commands{$_}{smry} ? $commands{$_}{smry} : $commands{$_}{what};
            $smry_text .= qq! ($commands{$_}{short}) ! 
                    if $commands{$_}{short};

            my $smry_code = qq!sub smry_$_ { q( $smry_text ) }; !; 
            $debug and print "evalcode: $smry_code\n";
            eval $smry_code;
            $debug and $@ and print "create_sub eval error: $@\n";

            my $alias_code = qq!sub alias_$_ { qw($commands{$_}{short}) }; !;
            $debug and print "evalcode: $alias_code\n";
            eval $alias_code;# noisy in docs
            $debug and $@ and print "create_sub eval error: $@\n";

        }

    grep{ $_ !~ /mark/ and $_ !~ /effect/ } keys %commands;

}
=cut
    


package Audio::Ecasound::Multitrack;

### COMMAND LINE PARSER 

$debug2 and print "Reading grammar\n";

$commands_yml = <<'YML';
---
help:
  short: h
  what: display help 
  parameters: topic or command name 
  type: general
exit:
  short: quit q
  what: exit program, saving settings
  type: general
getpos:
  type: transport
  short: gp
  what: get current playhead position (seconds)
  parameters: none
setpos:
  short: sp
  what: set current playhead position (seconds)
  example: setpos 65 (set play position to 65 seconds from start)
  type: transport
group_rec:
  type: group
  short: grec R
  what: rec-enable user tracks
  parameters: none
group_mon:
  type: group
  short: gmon M
  what: rec-disable user tracks
  parameters: none
group_off:
  type: group
  short: goff Z 
  what: group OFF mode, exclude all user tracks from chain setup
  smry: group OFF mode
  parameters: none
mixdown:
  type: mix
  short: mxd
  what: enable mixdown on subsequent engine runs
  smry: enable mixdown
  parameters: none
mixplay:
  type: mix
  short: mxp
  what: Play back mixdown file, with user tracks OFF
  smry: Playback mix
  parameters: none
mixoff:
  type: mix
  short: norm mxo normal
  what: normal mode, i.e. Mixdown group OFF, user tracks MON
  smry: mix off
  parameters: none
add_track:
  type: track
  short: add
  what: create a new track
  example: add sax; r2  (record track sax from input 2)
  parameters: string 
set_track:
  type: track
  what: directly set values in current track (use with care!)
  smry: set object fields
  example: set ch_m 5   (direct monitor output for current track to channel 5)
dump_track:
  type: track
  what: dump current track data to screen (YAML format)
  short: dump
  smry: dump track data
dump_group:
  type: group
  what: dump the settings of the group for user tracks 
  short: dumpg
  smry: dump group settings
rec:
  type: track
  what: rec enable 
  parameters: none
mon:
  type: track
  what: set track to MON
  parameters: none
off:
  type: track
  short: z
  what: set track OFF (exclude from chain setup)
  smry: set track OFF 
  parameters: none
monitor_channel:
  type: track
  short: m
  what: set track output channel number
  smry: set track output channel
  parameters: number
record_channel:
  type: track
  short: r
  what: set track input channel number
  smry: set track input channel 
set_version:
  type: track
  short: version n ver
  what: set track version number for monitoring (overrides group version setting)
  smry: select track version
  parameters: number
  example: sax; version 5; sh
group_version:
  type: group 
  short: gn gver gv
  what: set group version for monitoring (overridden by track-version settings)
  smry: get/set group version
list_versions:
  type: track
  short: lver lv
  what: list version numbers of current track
  smry: list track versions
  parameters: none
vol:
  type: track
  short: v
  what: get/set track volume, current track
  smry: get/set track volume
  parameters: number
mute:
  type: track
  short: c cut
  what: set playback volume to zero, current track
  smry: mute volume
  parameters: none
unmute:
  type: track
  short: cc uncut
  what: restore volume level before mute
unity:
  type: track
  what: set unity, i.e. 100 volume, current track
  smry: unity volume
  parameters: none
pan:
  type: track
  short: p	
  what: get/set pan position, current track
  smry: get/set pan position
  parameters: number
pan_right:
  type: track
  short: pr
  what: pan track fully right
  parameters: none
pan_left:
  type: track
  short: pl
  what: pan track fully left
  parameters: none
pan_center:
  type: track
  short: pc
  what: set pan center
  parameters: none
pan_back:
  type: track
  short: pb
  what: restore pan setting prior to pl, pr, or pc commands
  smry: restore pan
  parameters: none
save_state:
  type: project
  short: keep k save
  what: save project settings to disk, optional name
  parameters: optional string
get_state:
  type: project
  short: recall restore retrieve
  what: retrieve project settings
create_project:
  type: project
  short: create	
  what: create a new project directory tree
  example: create pauls_gig
  parameters: string
load_project:
  type: project
  short: load	
  what: load an existing project, or recall from last save
  smry: load project settings
  parameters: project_name
  example: load pauls_gig
generate:
  type: setup
  short: gen
  what: generate chain setup for audio processing
  parameters: none
arm:
  type: setup
  short: generate_and_connect
  what: generate and connect chain setup
  parameters: none
connect:
  type: setup
  short: con
  what: connect chain setup
  parameters: none
disconnect:
  type: setup
  short: dcon
  what: disconnect chain setup
  parameters: none
engine_status:
  type: setup
  what: ecasound audio processing engine status
  smry: engine status info
  short: egs
show_chain_setup:
  type: setup
  short: chains setup
  what: show current Ecasound chain setup
  smry: show chain setup
show_io:
  type: setup
  short: showio
  what: show chain input and output fragments
  smry: show chain fragments
show_tracks:
  type: setup
  short: show tracks
  what: show track status
show_track:
  type: track
  short: sh
  what: show track status, effects, versions
modifiers:
  type: track
  short: mods mod 
  what: set/show track modifiers 
  example: modifiers select 5 15.2 reverse playat 78.2 audioloop
nomodifiers:
  type: track
  short: nomods nomod
  what: remove modifiers from current track
show_effects:
  type: effect
  what: show effects on current track
  short: fxs sfx
  parameters: none
add_effect:
  type: effect
  what: add effect (Ecasound, Ecasound preset or LADSPA) to selected track
  short: fxa afx
  smry: add effect
  parameters: code param1 param2,... paramn
  example: fxa amp 6 (LADSPA Simple amp 6dB gain),\n         fxa var_dali (preset var_dali),\n         fxa pn:var_tape_stretched (explicit el: or pn: prefix optional)\n
modify_effect:
  type: effect
  what: modify an effect parameter
  parameters: effect_id, parameter, optional_sign, value
  short: fxm mfx
  example: fxm V 1 1000 (set to 1000), fxm V 1 -10 (reduce by 10) 
remove_effect:
  type: effect
  what: remove effects from selected track
  short: fxr rfx
  parameters: effect_id1, effect_id2,...
  example: fxr V (remove effect V)
helpx:
  what: hellow world
list_marks:
  type: mark
  short: lm
  what: List all marks
  parameters: none
to_mark:
  type: mark
  short: tom
  what: move playhead to named mark or mark index
  smry: playhead to mark
  parameters: string or integer
  example: tom start (go to mark named 'start')
mark:
  type: mark
  what: Mark current head position, becomes current mark
  smry: mark current position
  short: k	
  parameters: none
remove_mark:
  type: mark
  what: Remove mark 
  short: rmm
  parameters: mark name, mark index or none (for current mark) 
  example: rmm start (remove mark named 'start')
next_mark:
  type: mark
  short: nm
  what: Move playback head to next mark
  parameters: none
previous_mark:
  type: mark
  short: pm
  what: Move playback head to previous mark
  parameters: none
name_mark:
  type: mark
  short: nmk
  what: Give a name to the current mark
  parameters: string
  example: nmk start
remove_track:
  type: mark
  what: Make track go away (non destructive)
  parameters: string
  example: remove_track sax
stop:
  type: transport
  short: s
  what: stop transport
start:
  type: transport
  short: t
  what: start transport
loop_enable:
  type: transport
  short: loop
  what: playback will loop between marks
  parameters: start end 
  example: loop a b (loop between marks 'a' and 'b'),\n         loop 1 3 (loop between marks 1 and 3),\n         loop 13.6 69.0 (loop between times, decimal required) 
loop_disable:
  type: transport
  short: noloop nl
  what: disable automatic looping
T:
  type: transport
  what: ecasound-only start
S:
  type: transport
  what: ecasound-only stop
ctrl_register:
  type: effect
  what: list Ecasound controllers
  short: crg
preset_register:
  type: effect
  what: list Ecasound presets 
  short: prg
ladspa_register:
  type: effect
  what: list LADSPA plugins
  short: lrg
project_name:
  type: project
  what: show current project name
  short: project pn
...

YML

%commands = %{ Audio::Ecasound::Multitrack::yaml_in( $Audio::Ecasound::Multitrack::commands_yml) };

$Audio::Ecasound::Multitrack::AUTOSTUB = 1;
$Audio::Ecasound::Multitrack::RD_HINT = 1;

# rec command changes active take

$grammar = q(


key: /\w+/
someval: /[\w.+-]+/
sign: /[+-]/
op_id: /[A-Z]+/
parameter: /\d+/
value: /[\d\.eE+-]+/ 
last: ('last' | '$' ) 
dd: /\d+/
name: /[\w:]+/
modifier: 'audioloop' | 'select' | 'reverse' | 'playat' | value
nomodifiers: _nomodifiers end { $Audio::Ecasound::Multitrack::this_track->set(modifiers => ""); }
asdf: 'asdf' { print "hello"}
command: fail
end: /\s*$/ 
end: ';' 
help: _help end { print $Audio::Ecasound::Multitrack::help_screen }
help: _help name end { Audio::Ecasound::Multitrack::Text::help($item{name}) }



helpx: 'helpx' end { print "hello_from your command line gramar\n"; }
fail: 'f' end { print "your command line gramar will get a zero\n"; }
exit: _exit end { Audio::Ecasound::Multitrack::save_state(); exit }
create_project: _create_project name end {
	Audio::Ecasound::Multitrack::load_project( 
		name => Audio::Ecasound::Multitrack::remove_spaces($item{name}),
		create => 1,
	);
	print "created project: $Audio::Ecasound::Multitrack::project_name\n";

}

load_project: _load_project name end {
	my $untested = Audio::Ecasound::Multitrack::remove_spaces($item{name});
	print ("Project $untested does not exist\n"), return
	unless -d Audio::Ecasound::Multitrack::join_path Audio::Ecasound::Multitrack::project_root(), $untested; 
	Audio::Ecasound::Multitrack::load_project( name => Audio::Ecasound::Multitrack::remove_spaces($item{name}) );

	print "loaded project: $Audio::Ecasound::Multitrack::project_name\n";
}
save_state: _save_state name end { 
	Audio::Ecasound::Multitrack::save_state( $item{name} ); 
	}
save_state: _save_state end { Audio::Ecasound::Multitrack::save_state() }


get_state: _get_state name end {
	
 	Audio::Ecasound::Multitrack::load_project( 
 		name => $Audio::Ecasound::Multitrack::project_name,
 		settings => $item{name}
 		);
 
 	}
get_state: _get_state end {
	
 	Audio::Ecasound::Multitrack::load_project( 
 		name => $Audio::Ecasound::Multitrack::project_name,
 		);
 
 	}
getpos: _getpos end {  
	print Audio::Ecasound::Multitrack::d1( Audio::Ecasound::Multitrack::eval_iam q(getpos) ), $/; }
setpos: _setpos value end {
	Audio::Ecasound::Multitrack::eval_iam("setpos $item{value}");
}

add_track: _add_track name end { 
	
	Audio::Ecasound::Multitrack::add_track($item{name}); 
	
}


set_track: _set_track key someval end {
	 $Audio::Ecasound::Multitrack::this_track->set( $item{key}, $item{someval} );
}
dump_track: _dump_track { $Audio::Ecasound::Multitrack::this_track->dumpp }

dump_group: _dump_group { $Audio::Ecasound::Multitrack::tracker->dumpp }

 
remove_track: _remove_track name end {
	$Audio::Ecasound::Multitrack::tn{ $item{name} }->set(hide => 1); }

generate: _generate end { Audio::Ecasound::Multitrack::generate_setup(); }

arm: _arm end { 
	Audio::Ecasound::Multitrack::generate_setup() and Audio::Ecasound::Multitrack::connect_transport(); }

connect: _connect end { Audio::Ecasound::Multitrack::connect_transport(); }

disconnect: _disconnect end { Audio::Ecasound::Multitrack::disconnect_transport(); }


renew_engine: _renew_engine end { Audio::Ecasound::Multitrack::new_engine(); }
engine_status: _engine_status end { print(Audio::Ecasound::Multitrack::eval_iam
q(engine-status));print $/ }

start: _start end { Audio::Ecasound::Multitrack::start_transport(); }
stop: _stop end { Audio::Ecasound::Multitrack::stop_transport(); }

S: _S end { Audio::Ecasound::Multitrack::eval_iam("stop") }
T: _T end { Audio::Ecasound::Multitrack::eval_iam("start") }

show_tracks: _show_tracks end { 	

	Audio::Ecasound::Multitrack::Text::show_tracks ( Audio::Ecasound::Multitrack::Track::all );
	use warnings; 
	no warnings qw(uninitialized); 
	print $/, " " x 7, "Group", " " x 9, $Audio::Ecasound::Multitrack::tracker->rw, " " x 24 , $Audio::Ecasound::Multitrack::tracker->version, $/;
}


modifiers: _modifiers modifier(s) end {
 	 $Audio::Ecasound::Multitrack::this_track->set(modifiers => (join q(,),
	 @{$item{"modifier(s)"}}, q() ))
}

modifiers: _modifiers end { print $Audio::Ecasound::Multitrack::this_track->modifiers, $/; }
	
	
show_chain_setup: _show_chain_setup {
	my $chain_setup;
	Audio::Ecasound::Multitrack::io( Audio::Ecasound::Multitrack::join_path( Audio::Ecasound::Multitrack::project_dir(), $Audio::Ecasound::Multitrack::chain_setup_file) ) > $chain_setup; 
	print $chain_setup, $/;
}

show_io: _show_io { print Audio::Ecasound::Multitrack::yaml_out( \%Audio::Ecasound::Multitrack::inputs ),
Audio::Ecasound::Multitrack::yaml_out( \%Audio::Ecasound::Multitrack::outputs ); }

show_track: _show_track end {
	Audio::Ecasound::Multitrack::Text::show_tracks($Audio::Ecasound::Multitrack::this_track);
	Audio::Ecasound::Multitrack::Text::show_effects();
	Audio::Ecasound::Multitrack::Text::show_versions();
	Audio::Ecasound::Multitrack::Text::show_modifiers();
}
show_track: _show_track name end { 
 	Audio::Ecasound::Multitrack::Text::show_tracks( $Audio::Ecasound::Multitrack::tn{$item{name}} ) if $Audio::Ecasound::Multitrack::tn{$item{name}}
}
show_track: _show_track dd end {  
	Audio::Ecasound::Multitrack::Text::show_tracks( $Audio::Ecasound::Multitrack::ti[$item{dd}] ) if $Audio::Ecasound::Multitrack::ti[$item{dd}]
}
	



group_rec: _group_rec end { $Audio::Ecasound::Multitrack::tracker->set( rw => 'REC') }
group_mon: _group_mon end  { $Audio::Ecasound::Multitrack::tracker->set( rw => 'MON') }
group_off: _group_mute end { $Audio::Ecasound::Multitrack::tracker->set(rw => 'OFF') }

mixdown: _mixdown end { $Audio::Ecasound::Multitrack::mixdown_track->set(rw => 'REC')}
mixplay: _mixplay end { $Audio::Ecasound::Multitrack::mixdown_track->set(rw => 'MON');
						$Audio::Ecasound::Multitrack::tracker->set(rw => 'OFF');
}
mixoff:  _mixoff  end { $Audio::Ecasound::Multitrack::mixdown_track->set(rw => 'OFF');
						$Audio::Ecasound::Multitrack::tracker->set(rw => 'MON')}

record: 'record' end {} 

exit: 'exit' end { Audio::Ecasound::Multitrack::save_state($Audio::Ecasound::Multitrack::state_store_file); exit; }



r: 'r' dd  {	
				$Audio::Ecasound::Multitrack::this_track->set(ch_r => $item{dd});
				$Audio::Ecasound::Multitrack::ch_r = $item{dd};
				print "Input switched to channel $Audio::Ecasound::Multitrack::ch_r.\n";
				
				}
m: 'm' dd  {	
				$Audio::Ecasound::Multitrack::this_track->set(ch_m => $item{dd}) ;
				$Audio::Ecasound::Multitrack::ch_m = $item{dd};
				print "Output switched to channel $Audio::Ecasound::Multitrack::ch_m.\n";
				
				}

off: 'off' end {$Audio::Ecasound::Multitrack::this_track->set(rw => 'OFF'); }
rec: 'rec' end {$Audio::Ecasound::Multitrack::this_track->set(rw => 'REC'); }
mon: 'mon' end {$Audio::Ecasound::Multitrack::this_track->set(rw => 'MON'); }

wav: name { $Audio::Ecasound::Multitrack::this_track = $Audio::Ecasound::Multitrack::tn{$item{name}} if $Audio::Ecasound::Multitrack::tn{$item{name}}  }


set_version: _set_version dd end { $Audio::Ecasound::Multitrack::this_track->set(active => $item{dd})}
 
vol: _vol dd end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->vol }->[0] = $item{dd}; 
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->vol, 0);
} 
vol: _vol '+' dd end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->vol }->[0] += $item{dd};
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->vol, 0);
} 
vol: _vol '-' dd end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->vol }->[0] -= $item{dd} ;
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->vol, 0);
} 
vol: _vol end { print $Audio::Ecasound::Multitrack::copp{$Audio::Ecasound::Multitrack::this_track->vol}[0], $/ }

mute: _mute end {

	$Audio::Ecasound::Multitrack::this_track->set(old_vol_level => $Audio::Ecasound::Multitrack::copp{$Audio::Ecasound::Multitrack::this_track->vol}[0])
		if ( $Audio::Ecasound::Multitrack::copp{$Audio::Ecasound::Multitrack::this_track->vol}[0]);  
	$Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->vol }->[0] = 0;
	Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->vol, 0);
}
unmute: _unmute end {
	return if $Audio::Ecasound::Multitrack::copp{$Audio::Ecasound::Multitrack::this_track->vol}[0]; 
	return if ! $Audio::Ecasound::Multitrack::this_track->old_vol_level;
	$Audio::Ecasound::Multitrack::copp{$Audio::Ecasound::Multitrack::this_track->vol}[0] = $Audio::Ecasound::Multitrack::this_track->old_vol_level;
	$Audio::Ecasound::Multitrack::this_track->set(old_vol_level => 0);
	Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->vol, 0);
}


unity: _unity end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->vol }->[0] = 100;
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->vol, 0);
}

pan: _pan dd end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] = $item{dd};
	my $current = $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0];
	$Audio::Ecasound::Multitrack::this_track->set(old_pan_level => $current);
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->pan, 0);

} 
pan: _pan '+' dd end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] += $item{dd} ;
	my $current = $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0];
	$Audio::Ecasound::Multitrack::this_track->set(old_pan_level => $current);
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->pan, 0);
} 
pan: _pan '-' dd end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] -= $item{dd} ;
	my $current = $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0];
	$Audio::Ecasound::Multitrack::this_track->set(old_pan_level => $current);
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->pan, 0);
} 
pan: _pan end { print $Audio::Ecasound::Multitrack::copp{$Audio::Ecasound::Multitrack::this_track->pan}[0], $/ }

pan_right: _pan_right   end { 
	$Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] = 100;
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->pan, 0);
}
pan_left:  _pan_left end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] = 0; 
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->pan, 0);
}
pan_center: _pan_center end { $Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] = 50   ;
				Audio::Ecasound::Multitrack::sync_effect_param( $Audio::Ecasound::Multitrack::this_track->pan, 0);
}
pan_back:  _pan_back end {
	$Audio::Ecasound::Multitrack::copp{ $Audio::Ecasound::Multitrack::this_track->pan }->[0] = $Audio::Ecasound::Multitrack::this_track->old_pan_level;

}
remove_mark: _remove_mark dd end {
	my @marks = Audio::Ecasound::Multitrack::Mark::all();
	$marks[$item{dd}]->remove if defined $marks[$item{dd}];
}

remove_mark: _remove_mark name end { 
	my $mark = $Audio::Ecasound::Multitrack::Mark::by_name{$item{name}};
	$mark->remove if defined $mark;

}
	
remove_mark: _remove_mark end { 
	return unless (ref $Audio::Ecasound::Multitrack::this_mark) =~ /Mark/;
	$Audio::Ecasound::Multitrack::this_mark->remove;
}
	

mark: _mark end { $Audio::Ecasound::Multitrack::ui->marker( Audio::Ecasound::Multitrack::mark_here() )  }

next_mark: _next_mark end { Audio::Ecasound::Multitrack::next_mark() }

previous_mark: _previous_mark end { Audio::Ecasound::Multitrack::previous_mark() }

loop_enable: _loop_enable someval(s) end {
	my @new_endpoints = @{ $item{"someval(s)"}}; 
	
	$Audio::Ecasound::Multitrack::loop_enable = 1;
	@Audio::Ecasound::Multitrack::loop_endpoints = (@new_endpoints, @Audio::Ecasound::Multitrack::loop_endpoints); 
	@Audio::Ecasound::Multitrack::loop_endpoints = @Audio::Ecasound::Multitrack::loop_endpoints[0,1];
}
loop_disable: _loop_disable end {
	$Audio::Ecasound::Multitrack::loop_enable = 0;
}
	
name_mark: _name_mark name end {$Audio::Ecasound::Multitrack::this_mark->set_name( $item{name}) }

list_marks: _list_marks end { 
	my $i = 0;
	map{ print( $_->time == $Audio::Ecasound::Multitrack::this_mark->time ? q(*) : q()
	,join " ", $i++, sprintf("%.1f", $_->time), $_->name, $/)  } 
		  
		  @Audio::Ecasound::Multitrack::Mark::all;
	my $start = my $end = "undefined";
	print "now at ", sprintf("%.1f", Audio::Ecasound::Multitrack::eval_iam "getpos"), $/;

}
to_mark: _to_mark dd end {
	my @marks = Audio::Ecasound::Multitrack::Mark::all();
	$marks[$item{dd}]->jump_here;
}

to_mark: _to_mark name end { 
	my $mark = $Audio::Ecasound::Multitrack::Mark::by_name{$item{name}};
	$mark->jump_here if defined $mark;

}

show_effects: _show_effects end {}

remove_effect: _remove_effect op_id(s) end {
	
	map{ print "removing effect id: $_\n"; Audio::Ecasound::Multitrack::remove_effect( $_ )
	} grep { $_ }  @{ $item{"op_id(s)"}} ;
	

}


add_effect: _add_effect name value(s?)  end { 





my $code = $item{name};
if ( $Audio::Ecasound::Multitrack::effect_i{$code} ) {} 
elsif ( $Audio::Ecasound::Multitrack::effect_j{$code} ) { $code = $Audio::Ecasound::Multitrack::effect_j{$code} }
else { warn "effect code not found: $code\n"; return }
print "code: ", $code, $/;
	my %p = (
		chain => $Audio::Ecasound::Multitrack::this_track->n,
		values => $item{"value(s?)"},
		type => $code,
		);
		print "adding effect\n";
		
	Audio::Ecasound::Multitrack::add_effect( \%p );
}

modify_effect: _modify_effect op_id parameter sign(?) value end {

		
		$item{parameter}--; 

		my $new_value = $item{value}; 

		if ($item{"sign(?)"} and @{ $item{"sign(?)"} }) {
			$new_value = 
 			eval (join " ",
 				$Audio::Ecasound::Multitrack::copp{$item{op_id}}->[$item{parameter}], 
 				@{$item{"sign(?)"}},
 				$item{value});
		}
			
	Audio::Ecasound::Multitrack::effect_update_copp_set( 
		$Audio::Ecasound::Multitrack::cops{ $item{op_id} }->{chain}, 
		$item{op_id}, 
		$item{parameter}, 
		$new_value);

}
group_version: _group_version end { 
	use warnings;
	no warnings qw(uninitialized);
	print $Audio::Ecasound::Multitrack::tracker->version, $/ }
group_version: _group_version dd end { $Audio::Ecasound::Multitrack::tracker->set( version => $item{dd} )}


list_versions: _list_versions end { 
	print join " ", @{$Audio::Ecasound::Multitrack::this_track->versions}, $/;
}

ladspa_register: _ladspa_register end { print Audio::Ecasound::Multitrack::eval_iam("ladspa-register") }
preset_register: _preset_register end { print Audio::Ecasound::Multitrack::eval_iam("preset-register") }
ctrl_register: _ctrl_register end { print Audio::Ecasound::Multitrack::eval_iam("ctrl-register") }
project_name: _project_name end { print "project name: ", $Audio::Ecasound::Multitrack::project_name, $/ }


command: S
command: T
command: add_effect
command: add_track
command: arm
command: connect
command: create_project
command: ctrl_register
command: disconnect
command: dump_group
command: dump_track
command: engine_status
command: exit
command: generate
command: get_state
command: getpos
command: group_mon
command: group_off
command: group_rec
command: group_version
command: help
command: helpx
command: ladspa_register
command: list_marks
command: list_versions
command: load_project
command: loop_disable
command: loop_enable
command: mark
command: mixdown
command: mixoff
command: mixplay
command: modifiers
command: modify_effect
command: mon
command: monitor_channel
command: mute
command: name_mark
command: next_mark
command: nomodifiers
command: off
command: pan
command: pan_back
command: pan_center
command: pan_left
command: pan_right
command: preset_register
command: previous_mark
command: project_name
command: rec
command: record_channel
command: remove_effect
command: remove_mark
command: remove_track
command: save_state
command: set_track
command: set_version
command: setpos
command: show_chain_setup
command: show_effects
command: show_io
command: show_track
command: show_tracks
command: start
command: stop
command: to_mark
command: unity
command: unmute
command: vol
_S: 'S'
_T: 'T'
_add_effect: 'add_effect' | 'fxa' | 'afx'
_add_track: 'add_track' | 'add'
_arm: 'arm' | 'generate_and_connect'
_connect: 'connect' | 'con'
_create_project: 'create_project' | 'create'
_ctrl_register: 'ctrl_register' | 'crg'
_disconnect: 'disconnect' | 'dcon'
_dump_group: 'dump_group' | 'dumpg'
_dump_track: 'dump_track' | 'dump'
_engine_status: 'engine_status' | 'egs'
_exit: 'exit' | 'quit' | 'q'
_generate: 'generate' | 'gen'
_get_state: 'get_state' | 'recall' | 'restore' | 'retrieve'
_getpos: 'getpos' | 'gp'
_group_mon: 'group_mon' | 'gmon' | 'M'
_group_off: 'group_off' | 'goff' | 'Z'
_group_rec: 'group_rec' | 'grec' | 'R'
_group_version: 'group_version' | 'gn' | 'gver' | 'gv'
_help: 'help' | 'h'
_helpx: 'helpx'
_ladspa_register: 'ladspa_register' | 'lrg'
_list_marks: 'list_marks' | 'lm'
_list_versions: 'list_versions' | 'lver' | 'lv'
_load_project: 'load_project' | 'load'
_loop_disable: 'loop_disable' | 'noloop' | 'nl'
_loop_enable: 'loop_enable' | 'loop'
_mark: 'mark' | 'k'
_mixdown: 'mixdown' | 'mxd'
_mixoff: 'mixoff' | 'norm' | 'mxo' | 'normal'
_mixplay: 'mixplay' | 'mxp'
_modifiers: 'modifiers' | 'mods' | 'mod'
_modify_effect: 'modify_effect' | 'fxm' | 'mfx'
_mon: 'mon'
_monitor_channel: 'monitor_channel' | 'm'
_mute: 'mute' | 'c' | 'cut'
_name_mark: 'name_mark' | 'nmk'
_next_mark: 'next_mark' | 'nm'
_nomodifiers: 'nomodifiers' | 'nomods' | 'nomod'
_off: 'off' | 'z'
_pan: 'pan' | 'p'
_pan_back: 'pan_back' | 'pb'
_pan_center: 'pan_center' | 'pc'
_pan_left: 'pan_left' | 'pl'
_pan_right: 'pan_right' | 'pr'
_preset_register: 'preset_register' | 'prg'
_previous_mark: 'previous_mark' | 'pm'
_project_name: 'project_name' | 'project' | 'pn'
_rec: 'rec'
_record_channel: 'record_channel' | 'r'
_remove_effect: 'remove_effect' | 'fxr' | 'rfx'
_remove_mark: 'remove_mark' | 'rmm'
_remove_track: 'remove_track'
_save_state: 'save_state' | 'keep' | 'k' | 'save'
_set_track: 'set_track'
_set_version: 'set_version' | 'version' | 'n' | 'ver'
_setpos: 'setpos' | 'sp'
_show_chain_setup: 'show_chain_setup' | 'chains' | 'setup'
_show_effects: 'show_effects' | 'fxs' | 'sfx'
_show_io: 'show_io' | 'showio'
_show_track: 'show_track' | 'sh'
_show_tracks: 'show_tracks' | 'show' | 'tracks'
_start: 'start' | 't'
_stop: 'stop' | 's'
_to_mark: 'to_mark' | 'tom'
_unity: 'unity'
_unmute: 'unmute' | 'cc' | 'uncut'
_vol: 'vol' | 'v'
S: _S end { 1 }
T: _T end { 1 }
add_effect: _add_effect end { 1 }
add_track: _add_track end { 1 }
arm: _arm end { 1 }
connect: _connect end { 1 }
create_project: _create_project end { 1 }
ctrl_register: _ctrl_register end { 1 }
disconnect: _disconnect end { 1 }
dump_group: _dump_group end { 1 }
dump_track: _dump_track end { 1 }
engine_status: _engine_status end { 1 }
exit: _exit end { 1 }
generate: _generate end { 1 }
get_state: _get_state end { 1 }
getpos: _getpos end { 1 }
group_mon: _group_mon end { 1 }
group_off: _group_off end { 1 }
group_rec: _group_rec end { 1 }
group_version: _group_version end { 1 }
help: _help end { 1 }
helpx: _helpx end { 1 }
ladspa_register: _ladspa_register end { 1 }
list_marks: _list_marks end { 1 }
list_versions: _list_versions end { 1 }
load_project: _load_project end { 1 }
loop_disable: _loop_disable end { 1 }
loop_enable: _loop_enable end { 1 }
mark: _mark end { 1 }
mixdown: _mixdown end { 1 }
mixoff: _mixoff end { 1 }
mixplay: _mixplay end { 1 }
modifiers: _modifiers end { 1 }
modify_effect: _modify_effect end { 1 }
mon: _mon end { 1 }
monitor_channel: _monitor_channel end { 1 }
mute: _mute end { 1 }
name_mark: _name_mark end { 1 }
next_mark: _next_mark end { 1 }
nomodifiers: _nomodifiers end { 1 }
off: _off end { 1 }
pan: _pan end { 1 }
pan_back: _pan_back end { 1 }
pan_center: _pan_center end { 1 }
pan_left: _pan_left end { 1 }
pan_right: _pan_right end { 1 }
preset_register: _preset_register end { 1 }
previous_mark: _previous_mark end { 1 }
project_name: _project_name end { 1 }
rec: _rec end { 1 }
record_channel: _record_channel end { 1 }
remove_effect: _remove_effect end { 1 }
remove_mark: _remove_mark end { 1 }
remove_track: _remove_track end { 1 }
save_state: _save_state end { 1 }
set_track: _set_track end { 1 }
set_version: _set_version end { 1 }
setpos: _setpos end { 1 }
show_chain_setup: _show_chain_setup end { 1 }
show_effects: _show_effects end { 1 }
show_io: _show_io end { 1 }
show_track: _show_track end { 1 }
show_tracks: _show_tracks end { 1 }
start: _start end { 1 }
stop: _stop end { 1 }
to_mark: _to_mark end { 1 }
unity: _unity end { 1 }
unmute: _unmute end { 1 }
vol: _vol end { 1 }
);
open SAVERR, ">&STDERR";
open STDERR, ">/dev/null" or die "couldn't redirect IO";
$parser = new Parse::RecDescent ($grammar) or croak "Bad grammar!\n";
close STDERR;
open STDERR, ">&SAVERR";
#select STDOUT; $| = 1;
# Audio::Ecasound::Multitrack::Text::OuterShell::create_help_subs();
#

@help_topic = ( undef, qw(   
					project
					track
				    chain_setup
					transport
					marks
					effects
					group
					mixdown
					prompt 

				) ) ;

%help_topic = (

help => <<HELP,
   help <command>          - show help for <command>
   help <fragment>         - show help for commands matching /<fragment>/
   help <ladspa_id>        - invoke analyseplugin for info on a LADSPA id
   help <topic_number>     - list commands under <topic_number> 
HELP

project => <<PROJECT,
   load_project, load        - load an existing project 
   project_name, pn          - show the current project name
   create_project, create    - create a new project directory tree 
   get_state, recall, retrieve, restore  - retrieve saved settings
   save_state, keep, save    - save project settings to disk
   exit, quit                - exit program, saving state 
PROJECT

chain_setup => <<SETUP,
   setup, arm                - generate and connect chain setup    
   generate, gen             - generate chain setup
   connect, con              - connect chain setup
   show_setup, show          - show status, all tracks
   show_chain_setup, chains  - print .ecs file to STDOUT
SETUP
track => <<TRACK,
   add_track, add            -  create a new track, "add sax; r3"
                                (record sax from input 3) 

   show_tracks, show, tracks -  show status of all tracks
                                and group settings

   show_track, sh            -  show status of current track,
                                including effects and versions, 
                                "sax; sh"

   Most of the Track related commands operate on the 'current
   track'. To cut volume for a track called 'sax',  you enter
   'sax mute' or even 'sax; mute'. The first part of the
   command sets a new current track. You can also specify a
   current track by number,  i.e.  '4 mute'.

 - version

   set_version, version, ver, n  -  set current track version    

 - rw_status

   rec                     -  set track to REC  
   mon                     -  set track to MON
   off, z                  -  set track OFF (omit from setup)


 - vol/pan 

   pan, p                  -  get/set pan position
   pan_back, pb            -  restore pan after pr/pl/pc  
   pan_center, pc          -  set pan center    
   pan_left, pl            -  pan track fully left    
   pan_right, pr           -  pan track fully right    
   unity                   -  unity volume    
   vol, v                  -  get/set track volume    
   mute, c, cut            -  mute volume 
   unmute, uncut, cc       -  restore muted volume

 - channel assignments

   r, record_channel       -  set input channel number, current track
   m, monitor_channel      -  set output channel number, current track

 - chain object modifiers

   mod, mods, modifiers    - show or assign select/reverse/playat modifiers
                             for current track
   nomod, nomods, 
   nomodifiers             - remove all modifiers from current track
TRACK

transport => <<TRANSPORT,
   start, t           - Start processing
   stop, s            - Stop processing
   rewind, rw         - Rewind  some number of seconds, i.e. rw 15
   forward, fw        - Forward some number of seconds, i.e. fw 75
   setpos, sp         - Sets the current position, i.e. setpos 49.2
   getpos, gp         - Gets the current position 

   loop_enable, loop  - loop over marks, i.e. loop start finish
   loop_disable, noloop, nl -  disable looping
TRANSPORT

marks => <<MARKS,
   list_marks, lm     - list marks showing index, time, name
   next_mark, nm      - jump to next mark 
   previous_mark, pm  - jump to previous mark 
   name_mark, nom     - give a name to current mark 
   to_mark, tom       - jump to a mark by name or index
MARKS

effects => <<EFFECTS,
   ladspa-register, lrg       - list LADSPA effects
   preset-register, prg       - list Ecasound presets
   ctrl-register, crg         - list Ecasound controllers 
   add_effect,    fxa, afx    - add an effect to the current track
   modify_effect, fxm, mfx    - set, increment or decrement an effect parameter
   remove_effect, fxr, rfx    - remove an effect
EFFECTS

group => <<GROUP,
   group_rec, grec, R         - group REC mode 
   group_mon, gmon, M         - group MON mode 
   group_off, goff, MM        - group OFF mode 
   group_version, gver, gv    - select default group version 
GROUP

mixdown => <<MIXDOWN,
   mixdown, mxd                - enable mixdown 
   mixoff, mxo, normal, norm   - disable mixdown 
   mixplay, mxp                - playback a recorded mix 
MIXDOWN

prompt => <<PROMPT,
At the command prompt, you can enter several types
of commands:

   Type                        Example
   ------------------------------------------------------------
   Nama commands               load somesong
   Ecasound commands           cs-is-valid
   Shell expressions           ! ls
   Perl code                   eval 2*3     # no need for 'print'

PROMPT
	
);
# print values %help_topic;

$help_screen = <<HELP;
Welcome to Nama help

The help command can take several arguments.

help <command>          - show help for <command>
help <fragment>         - show help for all commands matching /<fragment>/
help <topic_number>     - list commands under topic <topic_number>

help is available for the following topics:

1  Project
2  Track
3  Chain setup
4  Transport
5  Marks
6  Effects
7  Group control
8  Mixdown
9  Command prompt 
10 All
HELP


# we use the following settings if we can't find config files

$default = <<'FALLBACK_CONFIG';
#
#
#         Configuration file for Audio::Multitrack

#         Notes: 

#         - This configuration file is distinct from
#           Ecasound's configuration file .ecasoundrc . 
#           In most instances the latter is not required.

#        - The format of this file is YAMLish, preprocessed to allow
#           comments.
#
#        - Indents are two spaces
#
#        - A value _must_ be supplied for each 'leaf' field
#          such as 'mixer_out_device'
#
#        - A value must _not_ be supplied for nodes, i.e.
#          'device', whose value is to be the entire indented
#          data structure that follows.
#
#        - The special symbol '~' represents a null value
#
#        - Check the 'devices' and 'abbreviations' section at
#          the end of this file to understand the values that
#          appear below. 'consumer' for example, is defined as
#          /dev/dsp.
#

project_root: ~ # null in default file

mixer_out_device: consumer

record_device: consumer

# globals for our chain setups

ecasound_globals: "-B auto -r -z:mixmode,sum -z:psr "

# audio formats 

mixer_out_format: cd-stereo
mix_to_disk_format: cd-stereo
raw_to_disk_format: cd-mono

# audio devices 

# indents _are_ significant in the lines below

devices: 
  jack: 
    ecasound_id: jack_alsa
    input_format: 32-12
    output_format: 32-10
  multi:
    ecasound_id: alsa,ice1712
    input_format: 32-12
    output_format: 32-10
  consumer:
    ecasound_id: /dev/dsp
    input_format: cd-stereo
    output_format: cd-stereo

# you may create arbitrary abbreviations

abbreviations:  
  24-mono: s24_le,1,frequency
  32-10: s32_le,10,frequency
  32-12: s32_le,12,frequency
  cd-mono: s16_le,1,44100
  cd-stereo: s16_le,2,44100,i
  frequency: 44100

FALLBACK_CONFIG

1;
__END__

=head1 NAME

B<Audio::Multitrack> - Perl extensions for multitrack audio processing

B<nama> - multitrack recording/mixing application

Type 'man nama' for details on usage and licensing.

No further documentation is provided regarding
Audio::Multitrack and its subordinate modules.
