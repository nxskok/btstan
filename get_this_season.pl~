use strict;
use warnings;
use 5.010;
use LWP::Simple;

main(@ARGV);

sub main {
    # england http://football-data.co.uk/mmz4281/1718/E0.csv
    my ($lg,$season)=@_;
    get_league($lg,$season);
}

sub get_league {
    my ($id,$season)=@_;
    my $url="http://football-data.co.uk/mnz4281/$season/$id.csv";
    getstore($url,"R/$id-$season.csv");
}
