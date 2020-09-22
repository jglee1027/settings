#!/usr/bin/env perl

# GitLab CLI tool that creates or updates a merge request
#
# Copyright (C) 2020  <jglee1027@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

use strict;
use warnings;
use utf8;

use HTTP::Tiny;
use JSON qw(decode_json encode_json);
use Term::Complete;
use Data::Dumper;

my $gitlab_token = env('GITLAB_TOKEN');
my $gitlab_user_id = env('GITLAB_USER_ID');
my $gitlab_api_url = env('GITLAB_URL') . "/api/v4/projects";
my $gitlab_mr_txt_file = "/tmp/gitlab_mr.txt";
my $gitlab_editor = "vim";

if (defined $ENV{GITLAB_EDITOR}) {
    $gitlab_editor = $ENV{GITLAB_EDITOR};
}

my $headers = {
    'PRIVATE-TOKEN' => $gitlab_token,
    'Content-Type' => "application/json"
};

my $mr_info = mr_info();

sub trim {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    return $s
};

sub env {
    my ($var) = @_;

    if (not defined $ENV{$var}) {
        print "Add \"$var\" environment variable!!!\n";
        exit 1;
    }
    return $ENV{$var};
}

sub mr_info {
    my $origin_url = trim(`git remote get-url origin`);
    if ($?) {
        exit 2;
    }

    my $http = HTTP::Tiny->new;
    my $page = 1;
    my $total_pages = 1;
    my $per_page = 100;

    my $res;

    do {
        my $response = $http->get($gitlab_api_url . "?page=$page&per_page=$per_page",
                                  { headers => $headers });
        $total_pages = $response->{headers}->{'x-total-pages'};
        my $decoded_json = decode_json($response->{content});

        foreach my $h (@$decoded_json) {
            if (trim($h->{ssh_url_to_repo}) eq $origin_url) {
                $res->{id} = $h->{id};
                $res->{url} = $h->{_links}->{merge_requests};
                $res->{default_branch} = $h->{default_branch};
                return $res;
            }
        }
        $page++;
    } while ($page <= $total_pages);

    if (not defined $res) {
        print "Not found \"$origin_url\" in $gitlab_api_url\n";
        print "Check the scopes of your personal access tokens in GitLab!\n";
        print "Please contact a GitLab administrator.\n";
        exit 3;
    }

    return $res;
}

sub update_mr {
    my ($mr_id, $params) = @_;

    my $http = HTTP::Tiny->new;
    my $url = $mr_info->{url} . "/" . $mr_id;
    my $response = $http->put($url, {headers => $headers,
                                     content => $params});

    print "$response->{status} $response->{reason}\n";
    my $decoded_json = decode_json($response->{content});
}

sub create_mr {
    my ($args) = @_;

    $args->{id} = $mr_info->{id};
    $args->{assignee_id} = $gitlab_user_id;
    $args->{remove_source_branch} = 'true';

    my $http = HTTP::Tiny->new;
    my $params = encode_json($args);
    my $response = $http->post($mr_info->{url}, {headers => $headers,
                                                 content => $params});

    print "$response->{status} $response->{reason}\n";
    my $decoded_json = decode_json($response->{content});

    if ($response->{status} == 409) {
        my $message = $decoded_json->{message};
        my ($mr_id) = (@$message[0] =~ /!(\d*)$/);
        update_mr($mr_id, $params);
    }
}

sub parse_gitlab_mr_txt {
    open(my $fh, "<:encoding(UTF-8)", $gitlab_mr_txt_file)
        or die "Could not open file '$gitlab_mr_txt_file' $!";

    my $title = '';
    my $desc = '';
    my $is_title = 1;

    while (my $line = <$fh>) {
        if ($line =~ /=== description\(\d*\) ===/) {
            $is_title = 0;
            next;
        }
        if ($is_title) {
            $title = $title . $line;
        } else {
            $desc = $desc . $line;
        }
    }

    return (trim($title), trim($desc));
}

sub prompt_target_branch {
    my @remote_branches = split "\n", `git branch -r`;
    shift @remote_branches;
    @remote_branches = map {my $s = trim($_);
                            $s =~ s/^origin\///g;
                            $s} @remote_branches;

    my $branch = Term::Complete::Complete('target_branch ? ', @remote_branches);
    return $branch;
}

sub target_branch {
    my $branch;
    if (defined $ENV{GITLAB_TARGET_BRANCH}) {
        $branch = $ENV{GITLAB_TARGET_BRANCH};
        if (trim($branch) eq "") {
            $branch = prompt_target_branch();
        }
    } else {
        $branch = $mr_info->{default_branch};
    }

    $branch =~ s/^\s*origin\///g;
    return $branch;
}

system($gitlab_editor, $gitlab_mr_txt_file);

my $source_branch = trim(`git rev-parse --abbrev-ref HEAD`);
my $target_branch = target_branch();
my ($title, $description) = parse_gitlab_mr_txt();

my $args = {
    source_branch => $source_branch,
    target_branch => $target_branch,
    title => $title,
    description => $description
};

create_mr($args);
