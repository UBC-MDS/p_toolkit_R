<h5 align="center">
  <br>
<img src="doc/pictures/p_toolkit_logo.png" alt="p_toolkit" width="200"></a>
<br>
</h5>

<h4 align="center">A toolkit for adjusting and visualizing p values</a>.</h4>

<h5 align="center">
Created by</a></h5>

<h4 align="center">

[Amy Goldlist](https://github.com/amygoldlist) &nbsp;&middot;&nbsp;
[Esteban Angel](https://github.com/estebanangelm) &nbsp;&middot;&nbsp;
[Veronique Mulholland](https://github.com/vmulholl)
</a></h4>

<br>
<h4 align="center">

[![GitHub forks](https://img.shields.io/github/forks/UBC-MDS/p_toolkit_R.svg?style=social)](https://github.com/UBC-MDS/p_toolkit_R/network)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![GitHub issues](https://img.shields.io/github/issues/UBC-MDS/p_toolkit_R.svg?style=social)](https://github.com/UBC-MDS/p_toolkit_R/issues)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![GitHub stars](https://img.shields.io/github/stars/UBC-MDS/p_toolkit_R.svg?style=social)](https://github.com/UBC-MDS/p_toolkit_R/stargazers)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![GitHub license](https://img.shields.io/github/license/UBC-MDS/p_toolkit_R.svg?style=social)](https://github.com/UBC-MDS/p_toolkit_R/blob/master/LICENSE)
</a></h4>


<h1></h1>
<h4 align="center">
  <a href="#key-features">Key Features</a> &nbsp;&nbsp;&nbsp;•&nbsp;&nbsp;&nbsp;
  <a href="#how-to-use">How To Use</a> &nbsp;&nbsp;&nbsp;•&nbsp;&nbsp;&nbsp;
  <a href="#install">Install</a> &nbsp;&nbsp;&nbsp;•&nbsp;&nbsp;&nbsp;
  <a href="#credits">Credits</a> &nbsp;&nbsp;&nbsp;•&nbsp;&nbsp;&nbsp;
  <a href="#related">Related</a> &nbsp;&nbsp;&nbsp;•&nbsp;&nbsp;&nbsp;
  <a href="#license">License</a>
</h4>
<h1></h1>

<br>

## Key Features

p_toolkit is a package designed to help adjust and visualize p-values when using multiple comparisons.  As computing power has become powerful enough to run hundreds or even thousands of statistical tests, it is important to look at small p-values and try to understand whether the result is small simply by chance, or whether it truly is significant.  There are many tools to help decide when to reject a Null hypothesis, which can control either:

*  The chance of committing a type 1 error (rejecting a null hypothesis given that it is true) on a single test
* The chance of committing at least one type 1 error in *m* tests
* The chance of a null hypothesis being true given that we have rejected it, the False Discovery rate (FDR)

We can use the p-values alone, or an adjustment method such as the Bonferroni  or the Benjamini-Hochberg (BH) methods.  We can also use visualization methods such as QQ-plots or a scatter plot of the p-values, to try and detect patterns.

This package aims to combine these methods in a simple-to-use format, which works by outputting dataframes, which contain results from several adjustment methods.

| Functions    | Description                                         | Inputs                           | Outputs                                               |
|--------------|-----------------------------------------------------|----------------------------------|-------------------------------------------------------|
| `p_methods`  | Bonferroni, BH summary of adjusted pvals            | df/vector, p-value column, alpha | DATAFRAME<br> variable, raw pval, adjusted pvals      |
| `p_adjust`   | Correction method specific output                   | df/vector, p-value column, alpha | raw pval, adjusted pval, Significance, Critical value |
| `p_plot`     | Summary plot comparing methods                      | `p_methods` df                     |                                                       |
| `p_qq`       | qq plot labeling per method the significant pvals   |                                  |                                                       |
| *`p_matrix`  | Confusion matrix with FDR (reverse of type I error) |                                  |                                                       |
| *`p_summary` | Summary listing  FDR rate                           |                                  |                                                       |
 \* Bonus functions for data with labels

## How To Use

## Install

## Credits

* README formatting inspiration from  [Markdownify](https://github.com/amitmerchant1990/electron-markdownify/blob/master/README.md#key-features)
* Badges by [Shields IO](https://shields.io/)
* Logo by [Devendra Karkar](https://www.iconfinder.com/dev-patel)


## Related

### Package Dependencies

### Similar Packages and Functions

R:

* [`fdrtool`](https://www.rdocumentation.org/packages/fdrtool/versions/1.2.15)

* [`stats::p.adjust`](https://www.rdocumentation.org/packages/stats/versions/3.4.3)

Python:

* [`statsmodels.stats.multitest`](http://www.statsmodels.org/dev/_modules/statsmodels/stats/multitest.html)

## License

[MIT License](https://github.com/UBC-MDS/p_toolkit_R/blob/master/LICENSE)

---
<h6 align="center">
Created by

[Amy Goldlist](https://github.com/amygoldlist) &nbsp;&middot;&nbsp;
[Esteban Angel](https://github.com/estebanangelm) &nbsp;&middot;&nbsp;
[Veronique Mulholland](https://github.com/vmulholl)
</a></h4>
