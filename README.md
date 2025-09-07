
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SinoMedminer

<!-- badges: start -->

<!-- badges: end -->

SinoMedminer的目标是提升中医药处方数据挖掘的效能，且提供美观的可视化输出。用户可以Shiny应用进行快速分析，也可以通过函数指令进行更强的分析。

本研究成果已在国际核心期刊 *Chines Medicine* 发表，发表地址：DOI: 10.1186/s13020-025-01127-9.



## 安装

可以通过以下方式进行安装 [GitHub](https://github.com/) 上的版本：

``` r
# install.packages("devtools")
devtools::install_github("Tianyi-TCM/SinoMedminer")
```

## 功能介绍

R包主要包括以下大功能：

数据清洗、中药属性与功效分析、聚类分析、关联规则分析、处方相似度分析、共现网络分析、综合流向分析。

``` r
library(SinoMedminer)
#> Registered S3 methods overwritten by 'ggalt':
#>   method                  from   
#>   grid.draw.absoluteGrob  ggplot2
#>   grobHeight.absoluteGrob ggplot2
#>   grobWidth.absoluteGrob  ggplot2
#>   grobX.absoluteGrob      ggplot2
#>   grobY.absoluteGrob      ggplot2
## basic example code
```

介绍内容较多，静待更新……

在线的Shiny版本可以访问: SinoMedminer (formulaharmony.com)

## 数据准备
是最常见，最原始的格式。部分医院导出的数据为长格式数据。第1列为id列，其余列可以是中药，症状等，要求1个元素占一个单元格。要求列名不能重复。列名可以是：中药1、中药2、中药3、证候1、证候2、症状1、症状2……。中药元素可以包含剂量，也可以不包含剂量。示例如下：

![](https://secure2.wostatic.cn/static/xxFFeZCdNzBfEzHRTtLJFT/image.png?auth_key=1756908375-gzteFiWhJX9QH9gpv95Vcp-0-f517374d3e9e82a5d4749ba494079d51)

剂量问题：中药元素可以包含剂量，也可以不包含剂量。如果包含剂量，将所有剂量类型转换为剂量的克重，并且仅保留剂量数值，不要单位。例如“三七粉1袋”，更改为“三七粉3”。类似的还有“生姜5片”、“大枣6枚”、“白酒20ml”、“蜈蚣2条”。对于古代数据，还可能存在“钱”、“铢”、“两”、“斛”等单位。

整洁数据：若下文无特殊声明，则默认为第1列为id列，其余列为仅包含0与1的列。后文示例中大多整洁数据框由函数`trans_wide`转换而来，好处是会自动按照频数降序对列进行排序。示例如下：

![](https://secure2.wostatic.cn/static/3dBs9VwBsHJECwF37Q2j2q/image.png?auth_key=1756908375-i2GCaMfkJYPVSzxnbP1yeW-0-7cda7da71830f9e8876377dde6588460)

建议：如果id列是简单的数字连号，即1,2,3,4,5,6……，务必保持一致

### 文件格式

均为Excel表格，后缀名为“.xlsx”。

## 数据清洗与转换类函数
### makelookup2
**说明：**

根据输入的基本格式数据，将每个字段与标准数据集进行对比，获得需要规范的字段。至少有2列，且第1列为id。中药或饮片标准名参考药典、医保基金支付目录、“十四五规划”中药学教材目录，合计1304种名称。需要注意的是一个鉴于炮制方法与饮片差异，一个中药可能有多个名字。例如党参，有“党参”、“党参片”、”“米炒党参”、“明党参”等。

**用法：**
```R
makelookuptable2(df, type = NULL)
```

**参数：**

- df：基本格式的数据，是必须有的。
- type：字符串类型，有"Chinese"和"letters"两种选项，默认为 “Chinese” 。如果是“Chinese”则删除所有空格，如果是"letters"，将连续的空格变为1个空格。（？这里放一个数据清洗功能似乎不合适吧？本来就应该保持数据的全貌，如果数据是中文，一般考虑数据在导入R之前都会自行删除空格，如果是英文需要删除首尾的空格，并且将字段内部的连续空格变成1个空格，这个函数记得写过，忘记在哪了。）

**输出：**
返回一个包含3列的数据框：待标准化的名称、用于填写标准化名称的列，以及对应的频数。


## 关联规则函数
### 关联规则分析

#### trans_rules  

  **说明：**
  将整洁矩阵转换为关联规则对象

  **用法**：
  `trans_rules(df)`

  **参数**：

  - `df`：整洁数据框，第1列为id，其余列为仅包含0与1的列。

  **输出：**

  - 关联规则的`transactions`对象

  **示例：**

```R
trans_herb <- trans_rules(herb_wide)
```

  

## Shiny平台

因为网站已多次遭到不良攻击，因此分析功能需联系邮箱dan9521@foxmail.com

提供自我介绍和联系方式，开通使用。
