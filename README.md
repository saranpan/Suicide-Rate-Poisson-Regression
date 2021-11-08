

![suicide-fish](https://c.tenor.com/clmSODIfyZcAAAAC/suicide-fish.gif)


### Features


## Table of contents

- [Introduction](#Introduction)
- [Data](#Data)
- [What's included](#whats-included)
- [Bugs and feature requests](#bugs-and-feature-requests)
- [Contributing](#contributing)
- [Creators](#creators)
- [Thanks](#thanks)
- [Copyright and license](#copyright-and-license)


## Introduction

Nowaday, the suicide prevention campaign are now at every country which raise awareness, since the suicide count is rising exponentially. However, knowing the causality level of suicidal is a major key to prevent it. 

with this problem, data is very useful we can use to identify the cause, so our team decided to find the dataset about suicide statistics,and the dataset we found out is from World Health Organization (WHO), which contain the count of death person from suicide for each country.

Each observation in this dataset contains country,year,sex,age, and population which we can use to predict suicide count

However, our limitation is only the country that belongs to Asia will be inspected, that is why we will filter the original dataset to only Asian countries (which we will filter via Python)

---
## Data
As we mentioned before that we use the dataset collected by WHO, so here is s the original dataset : [WHO Suicide Statistics](https://www.kaggle.com/szamil/who-suicide-statistics)

**Explanatory variable** :
1. *Country* : there are 27 countries eg. Albania, Thailand, Japan (categorical)
2. *Year* : The time period happens eg. 1986, 2012 (numerical)
3. *Sex*: Male and female (categorical)
4. *Age* : The age range of the population eg. 15-24 years, 75+ years (categorical)
5. *Population* : The number of population of that country (numerical)
<br>
**Response variable** : 
1. *Suicides_no* : suicide count (numerical)

![figure1](https://github.com/wallik2/Suicide_rate_Poisson_regression/blob/main/figure/fig1.png?raw=true)
<center>Figure 1 : first ten rows of the data</center>

<br>

**How we clean the data before we use**
1. Drop all rows that have missing value (NA); with the following R code

```sh
print('Total missing value : ',sum(is.na(data)))
>> 7716
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

print('missing value of each column')
print(na_count)
```
## What's included

Some text



## Bugs and feature requests

Have a bug or a feature request? Please first read the [issue guidelines](https://reponame/blob/master/CONTRIBUTING.md) and search for existing and closed issues. If your problem or idea is not addressed yet, [please open a new issue](https://reponame/issues/new).

## Contributing

Please read through our [contributing guidelines](https://reponame/blob/master/CONTRIBUTING.md). Included are directions for opening issues, coding standards, and notes on development.

Moreover, all HTML and CSS should conform to the [Code Guide](https://github.com/mdo/code-guide), maintained by [Main author](https://github.com/usernamemainauthor).

Editor preferences are available in the [editor config](https://reponame/blob/master/.editorconfig) for easy use in common text editors. Read more and download plugins at <https://editorconfig.org/>.

## Creators

**Creator 1**

- <https://github.com/usernamecreator1>

## Thanks

Some Text

## Copyright and license

Code and documentation copyright 2011-2018 the authors. Code released under the [MIT License](https://reponame/blob/master/LICENSE).

Enjoy :metal:

```

###End
