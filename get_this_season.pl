use strict;
use warnings;
use 5.010;
use LWP::Simple;

main(@ARGV);

sub main {
    get_league("E0","1718");
    get_league("E1","1718");
    get_league("E2","1718");
    get_league("SC0","1718");
    get_league("SC1","1718");
    get_league("D1","1718");
    get_league("D2","1718");
    get_league("I1","1718");
    get_league("I2","1718");
    get_league("SP1","1718");
    get_league("SP2","1718");
    get_league("F1","1718");
    get_league("F2","1718");
    get_league("G1","1718");
}

sub get_league {
    my ($id,$season)=@_;
    my $url="http://football-data.co.uk/mnz4281/$season/$id.csv";
    getstore($url,"R/$id-$season.csv");
}
