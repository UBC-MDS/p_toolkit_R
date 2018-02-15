src for source code

| Functions    | Description                                         | Inputs                           | Outputs                                               |
|--------------|-----------------------------------------------------|----------------------------------|-------------------------------------------------------|
| [`p_methods`](https://github.com/UBC-MDS/ptoolkit/blob/master/R/p_methods.R)  | Bonferroni, BH summary of adjusted pvals            | df/vector, p-value column, alpha | DATAFRAME<br> variable, raw pval, adjusted pvals      |
| [`p_adjust`](https://github.com/UBC-MDS/ptoolkit/blob/master/R/p_adjust.R)   | Correction method specific output                   | df/vector, p-value column, alpha | raw pval, adjusted pval, Significance, Critical value |
| [`p_plot`](https://github.com/UBC-MDS/ptoolkit/blob/master/R/p_plot.R)     | Summary plot comparing methods (sample from DSCI553 Lecture 2)       | `p_methods` dataframe                  | ![](doc/pictures/sample-p_plot.PNG)![](doc/pictures/sample-p_plot-zoom.PNG) |
| [`p_qq`](https://github.com/UBC-MDS/ptoolkit/blob/master/R/p_qq.R)       | qq plot labeling per method the significant pvals   |      `p_methods` dataframe                                     |  ![](doc/pictures/sample-p_qq.PNG)![](doc/pictures/sample-p_qq-zoom.PNG) |
