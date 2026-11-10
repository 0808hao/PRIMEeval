# PRIMEeval 0.8.0.9001 中文使用说明

本候选版加入第93–94步的确定性审计策略。沿用 0.7.0 默认策略时，使用
WIDTH抽样、OFFSET在线预测、LEGACY证据和SUPPORT任务分配。
每任务先查询2个单元，总成本的一半用于前期筛查、另一半用于确认。
它用于在有限标签预算内发现损失差超过阈值的任务，不是一个重新训练的临床预测模型。

## 1. 纯R安装

将PRIMEeval_0.8.0.9001.tar.gz直接放到“二周目”目录，在R中运行：

```r
install.packages("/media/desk16/iy56901/nature决战/二周目/PRIMEeval_0.8.0.9001.tar.gz",
                 repos = NULL, type = "source")
library(PRIMEeval)
ex <- prime_example()
s <- prime_start(ex$tasks, budget = 8)
s <- prime_run(s, ex$query)
prime_summary(s)
```

该tar.gz本身就是标准R源码安装包，直接安装即可。额外依赖仅digest；stats和utils由R自带；
不需要编译器、其他语言、原实验脚本或服务器专有目录。若没有系统库写权限，
可以先创建个人R库，再用install.packages(..., lib=个人库目录)安装。

## 2. 公开输入与目标量

一个任务包含若干有限总体单元。每个单元的真实值是“候选模型损失－参考模型损失”。
数值为正表示候选模型的损失更大。一个单元也可以是预先定义的记录组，
但此时支持界和实际查询值必须对应同一个组级损失差。

公开数据框必需列：

| 列 | 含义 |
|---|---|
| support_lower | 该单元真实损失差的已知有效下界 |
| support_upper | 该单元真实损失差的已知有效上界 |
| proxy_generic | 公开代理预测，应来自查询前可获得的信息 |
| unit_id | 可选，唯一单元编号；省略时按行编号 |
| n_audit_records | 可选，查询成本，默认1，至少为1 |

可选代理列：proxy_signed_confidence、proxy_frozen_disagreement、proxy_relation、
proxy_relation_prior、exposure_score、uncertainty_score、source_id。
这些列均不能来自未查询的目标标签。

```r
task <- prime_task(public_units, task_id = "model_A_vs_reference", dataset = "site_A")
```

目标量是各单元损失差的等权平均。n_audit_records只表示成本，不自动改变目标量权重。
默认阈值threshold=0.01。支持界必须在目标标签揭示之前有效；包能检查数值越界，
无法替使用者证明外部提供的界是否正确。

## 3. 预算含义

budget=8表示每个数据集分别形成总成本上限：
所有任务的“平均单元成本 × min(总体单元数, 8)”之和。
任务间可以调配预算，所以单个任务可能查询超过8次。
不同数据集各自核算预算；cost_budget可显式指定数据集总成本。
max_queries默认32，限制每任务最大查询长度；allow_full_census=FALSE禁止查询该任务最后一个单元。
为与既定实验成本口径一致，在存在可支付的查询时，默认分配仍可能继续消耗剩余预算。

## 4. 历史拟合和校准

先前342/357和350/357是特定数据、历史拆分、拟合和逐步校准共同产生的开发结果，
不是任意新数据的保证。复现时须复用相同历史对象、任务ID、顺序、阈值、种子与最大查询长度。

```r
history <- prime_history(fit = frozen_fit, cuts = frozen_cuts,
                         threshold = 0.01, seed = 820081L, max_queries = 32L,
                         training_ids = historical_training_ids,
                         calibration_ids = historical_calibration_ids)
task <- prime_task(public_units, "new_target", "site_A", history = history)
```

frozen_fit可以直接使用原第82步FIT工作块的value；只保留所需variance和prior。
cuts可以使用原匹配的校准表，包会选择PLUGIN_IMPUTE/ERROR_Q。
若有历史全标签数据，可使用training和calibration两个独立任务家族列表拟合：
每项为list(task=prime_task(...), values=历史真实损失差向量)。
这只拟合历史方差模型和筛查阈值，不训练原候选/参考模型。

没有历史信息时允许启动，结果中history_scope明确标记UNCALIBRATED_PUBLIC_PRIOR。
示例属于该模式，仅验证接口可运行，不能用来声称复现旧实验效果。

## 5. 逐次查询与继续运行

```r
s <- prime_start(tasks, budget = 8)
z <- prime_next(s)
s <- z$state
req <- z$request
# 向你自己的数据源查询req指定的唯一单元：
y <- query(req$task_id, req$unit_id, req$dataset)
s <- prime_update(s, req, y)
```

请求包含单元、查询概率、成本及token。一次仅有一个待完成请求。
重复prime_next返回同一待完成请求；重复更新、修改token或标签越界会报错。
不要跳过请求并把另一个样本的值传进去。prime_run封装上述循环；
其query回调只需返回当前请求单元的一个数值。

saveRDS(s, ...)与readRDS(...)可保存整个状态，包含待完成请求；
真实数据源及回调由使用者独立保管。prime_trace和prime_log用于查看已查询轨迹与成本记录。

## 6. 历史增强学习：实验选项

```r
s <- prime_start(tasks, budget = 8, scheduler = "history_value",
                 value_models = models_by_task_id)
```

models_by_task_id是以目标task_id命名的固定模型列表；支持第88步MODEL工作块中
LEARN_AUG的模型结构。新包加载其参数进行排序，不重新训练该模块。
模型需要feature_names、cuts、lookup、intercept、range_min、range_max、usable；
有training_task_ids和selection_task_ids时会额外检查目标家族隔离。
若同一数据集有模型不可用，会整批明确回退SUPPORT。

第88步该选项的稀疏增益主要出现在Sleep_EDF构造场景；普通场景和异域历史可能退步。
它不是默认方法，也不会根据目标真值或场景标签自动选取最优方案。

## 7. 如何读结果

- support_only：与旧实验一致的查询后支持界确认标记。
- bound_certificate：直接由当前支持下界判断的确定性风险确认，零查询时也可成立。
- support_lower、support_upper：当前已查询信息与公开界构成的有限总体支持区间。
- formal_current、formal_anytime、formal_ebh：保留旧名称以便核对的开发性统计读数。
  其中eBH只使用当前e值，单任务历史峰值另列；不能混用。
- used、record_cost、full_census：查询数、实际成本、是否完成全部查询。
- history_scope、inferential_scope：历史信息及结论范围。

未确认不等于安全或不存在风险。跨任务自适应停止、方案选择后的整体错误控制，
以及患者级外部泛化尚未由本包建立。任务家族隔离不等于原始记录或患者隔离。

## 8. 版本与复核

0.7.0交付的是第85—88步方法的独立实现。旧0.6.0中其他扰动/重训练接口不包含在本版本内；
如需保留这些接口，可把两个版本安装到不同的个人R库。
包内含base-R回归检查和可运行示例。安装后可以直接运行：

```r
source(system.file("examples", "basic_audit.R", package = "PRIMEeval"), encoding = "UTF-8")
```


## 候选版的三种审计策略

现有 `prime_task()`、`prime_next()`、`prime_update()`、`prime_run()` 接口
可直接配合新版策略使用。以下以包内示例演示调用方式。

```r
ex <- prime_example()
ids <- vapply(ex$tasks, function(task) task$task_id, character(1))
# 示例中每个任务单独成组。实际使用时按预先确定的公开任务家族填写。
families <- stats::setNames(ids, ids)
state <- prime_start(ex$tasks, budget = 8, scheduler = "guarded_affine",
                     task_families = families)
state <- prime_run(state, ex$query)
prime_summary(state)
prime_gate(state)
prime_log(state)
```

- `affine_full` 对应第93步仿射续查。`bound_greedy` 是同一查询顺序下的边界贪心对照。
- `guarded_affine` 对应第94步保护策略。首个非先导请求前按数据集评估一次。
  至少 16 个家族且至少 80% 的初始合格任务完成两次先导查询时，
  若家族均值的 Spearman 相关不大于 -0.20，切换至边界贪心。
- 先导查询计入预算。三种策略默认可查至整个有限任务池，已由有效界限
  判定风险或排除风险的任务会提前停止。用 `max_queries` 可另设单任务上限。
- 三种策略只输出确定性证书和调度诊断。正式新任务池验证使用冻结后的同一源码包。
