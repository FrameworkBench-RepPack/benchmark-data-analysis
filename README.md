# Data Analysis Scripts
The repository contains the R scripts needed to run the data analysis.

## File Description
The project contains three files, each containing a step in the data analysis.

[The Cochrans file](https://github.com/FrameworkBench-RepPack/benchmark-data-analysis/blob/main/Cochrans.R) runs Cochran's formula on the data. This can be done with a smaller subset to determine the number of repetitions.

[The ks-test file](https://github.com/FrameworkBench-RepPack/benchmark-data-analysis/blob/main/ks-test.R) runs the One-sample Kolmogorov-Smirnov test. This is just to verify that the data is not normally distributed.

[The mann-whitney-u-one-sided file](https://github.com/FrameworkBench-RepPack/benchmark-data-analysis/blob/main/mann-whitney-u-one-sided.R) runs the one-sided Mann-Whitney U on the raw data. It runs a Kruskal-Wallis test as part of the Mann-Whitney U. It uses Holm to correct the P-values.

## Replication
To replicate the paper's results, upload [the raw data](https://github.com/FrameworkBench-RepPack/replication-package/data/raw-results) in the root of the R script project.

Cochran's formula was used on the raw data and not a smaller subset. We ran it for 45 iterations before discovering that we only needed 38.
