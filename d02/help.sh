#!/usr/bin/env sh
awkcommand='
BEGIN { sum=0; }
{
min_r=0;min_g=0;min_b=0;
for (i=1;i<=NF;i++) {
    if ($i=="red") {
        min_r=(min_r<$(i-1))?$(i-1):min_r;
    }
    else if ($i=="green") {
        min_g=(min_g<$(i-1))?$(i-1):min_g;
    }
    else if ($i=="blue") {
        min_b=(min_b<$(i-1))?$(i-1):min_b;
    }
}
#printf("%d\t%d\t%d\n", min_r, min_g, min_b);
sum+=min_r*min_g*min_b;
}
END { print sum }
'
awk -F',|;|:| ' "$awkcommand" input.txt
