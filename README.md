
# queryBuilder

[![version](https://img.shields.io/static/v1.svg?label=github.com&message=v.0.1.0&color=ff69b4)](https://r-world-devs.github.io/queryBuilder/)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

`queryBuilder` provides syntax for defining complex filtering
expressions in a programmatic way.  
Filtering query, built as a nested list configuration, can be easily
stored in other formats like ‘YAML’ or ‘JSON’. The package also allows
to convert such configuration to a valid expression that can be applied
with popular ‘dplyr’ package operations.

### Rules

The package allows to construct queries using **rules** (`queryRule`)
that are filtering operations performed on a single query.  
A single rule consists of:

- `field` - name of the variable/column to be filtered,
- `operator` - name of the filtering function to apply to the `field`,
- `value` - non-mandatory value precising the filtering by the operator.

As an example:

``` r
queryRule(
  field = "am",
  operator = "equal",
  value = 1
)
```

by the default package configuration, is interpreted as `am == 1`
expression.

In order to convert a rule to expression use `queryToExpr` function:

``` r
my_query <- queryRule(
  field = "am",
  operator = "equal",
  value = 1
)
queryToExpr(my_query)
#> am == 1
```

Such expression can be then used by `dplyr::filter`:

``` r
mtcars %>% dplyr::filter(!!queryToExpr(my_query))
#> # A tibble: 13 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6 160     110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6 160     110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 4  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 5  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> # ℹ 8 more rows
```

To see a full list of supported operators (along with corresponding R
functions) check:

``` r
listQueryOperators()
#> equal: ==
#> not_equal: !=
#> in: %in%
#> not_in: Negate(`%in%`)
#> less: <
#> less_or_equal: <=
#> greater: >
#> greater_or_equal: >=
#> between: queryBuilder::in_range
#> not_between: Negate(queryBuilder::in_range)
#> begins_with: startsWith
#> not_begins_with: Negate(startsWith)
#> contains: queryBuilder::in_string
#> not_contains: Negate(queryBuilder::in_string)
#> ends_with: endsWith
#> not_ends_with: Negate(endsWith)
#> is_empty: queryBuilder::is_empty
#> not_is_empty: Negate(queryBuilder::is_empty)
#> is_null: is.na
#> not_is_null: Negate(is.na)
```

More detailed description of supported operators can be found at
`vignette("operators")`.

You can also define custom operators with `setQueryOperators()`.

### Groups

To build more complex queries `queryBuilder` introduces **groups**
(`queryGroup`) that allow to combine multiple rules with the specified
**condition** (logical operator).

The below query:

``` r
my_query <- queryGroup(
  condition = "AND",
  queryRule(
    field = "am",
    operator = "equal",
    value = 1
  ),
  queryRule(
    field = "vs",
    operator = "equal",
    value = 0
  )
)
```

uses `"AND"` condition to combine the two rules which is by default
interpreted as `&` logical operator:

``` r
queryToExpr(my_query)
#> am == 1 & vs == 0
```

`queryGroup` can also combine other groups which enables to build even
more advanced queries:

``` r
my_query <- queryGroup(
  condition = "AND",
  queryRule("qsec", "greater", 20),
  queryGroup(
    condition = "OR",
    queryRule(
      field = "am",
      operator = "equal",
      value = 1
    ),
    queryRule(
      field = "vs",
      operator = "equal",
      value = 0
    )
  )
)
queryToExpr(my_query)
#> qsec > 20 & (am == 1 | vs == 0)
```

By default the packages supports two conditions `AND` (`&`) and `OR`
(`|`) but you can add your custom one with `setQueryConditions()`.

### Relation to [jQuery-QueryBuilder](https://github.com/mistic100/jQuery-QueryBuilder)

The introduced syntax (rules, groups, operators and conditions) is based
on query constructing rules as offered by
[jQuery-QueryBuilder](https://github.com/mistic100/jQuery-QueryBuilder)
JS framework.

The `queryBuilder` package is intended to be used as a backend for the
`shinyQueryBuilder` package that will allow users to use
`jQuery-QueryBuilder` in Shiny.

## Installation

``` r
# CRAN version
install.packages("queryBuilder")

# Latest development version
remotes::install_github("https://github.com/r-world-devs/queryBuilder")
```

## Acknowledgement

Special thanks to [Kamil Wais](mailto:kamil.wais@gmail.com), [Adam
Foryś](mailto:adam.forys@gmail.com), [Maciej
Banaś](mailto:banasmaciek@gmail.com),[Karolina
Marcinkowska](mailto:karolina_marcinkowska@onet.pl) and [Kamil
Koziej](mailto:koziej.k@gmail.com) for the support in the package
development and thorough testing of its functionality.

## Getting help

In a case you found any bugs, have feature request or general question
please file an issue at the package
[Github](https://github.com/r-world-devs/queryBuilder/issues). You may
also contact the package author directly via email at
<krystian8207@gmail.com>.
