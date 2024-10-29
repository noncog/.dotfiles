#!/usr/bin/perl
# Modified from: https://www.reddit.com/r/i3wm/comments/hbs0da/define_applicationspecific_keybinds/
use AnyEvent;
use AnyEvent::I3 qw(:all);
my $i3 = i3();

$i3->connect->recv or die "Error connecting";

my %callbacks = (
    window => sub {
        my ($x) = @_;
        my $ev = $x->{change};
        my $class = $x->{container}->{window_properties}->{class};

        if ($ev eq "focus") {
            if ($class eq "Emacs") {
                $i3->message(TYPE_COMMAND, "mode Emacs"); }
            elsif ($class eq "GT: New Horizons 2.6.1") {
                $i3->message(TYPE_COMMAND, "mode GTNH"); }
            else {
                $i3->message(TYPE_COMMAND, "mode default"); }
        }
    }
    );

$i3->subscribe(\%callbacks)->recv->{success}
        or die "Error subscribing";

AE::cv->recv