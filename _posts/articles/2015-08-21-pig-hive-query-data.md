---
layout: post
title: "Pig + Hive: Querying Processed Data"
excerpt: Big data analysis pipeline discussion.
categories: articles
tags: [data,bigdata,pig,hive,datawarehouse,warehouse,java,hadoop,mapreduce,apache,sql,hiveql,piglatin]
comments: true
share: true
ads: true
redirect_from: ['/post/127234735939/pig-hive-query-data/', '/post/127234735939/']
---

<p>Big data. Its a thing nowadays. Assuming you have more data than can fit in your standard SQL database (MSSQL, MySQL, Oracle, etc.) easily, then you probably need a way to get at all that stuff youre holding on to. After all, data is only as good as the insights which are drawn from it. Today well be discussing a bit about <a href="https://pig.apache.org/" target="_blank">Pig</a> and <a href="https://hive.apache.org/" target="_blank">Hive</a>. When these two tools are brought up, people generally are asking what is the difference and when to use one over the other. However, were going to look at using them together to build more powerful systems.</p><h2>Pig</h2><p>In a very brief summary, Pig is simply a scripting language (well, PigLatin is technically the language) that creates and launches <a href="https://hadoop.apache.org/" target="_blank">Hadoop</a> MapReduce jobs. The concept of <a href="https://en.wikipedia.org/wiki/User-defined_function" target="_blank">UDFs</a> carries over from databases and Pig scripts can be augmented with functions written in Java. Typically, Pig jobs consume input and deliver output via <a href="http://hadoop.apache.org/docs/current/hadoop-project-dist/hadoop-hdfs/HdfsUserGuide.html" target="_blank">HDFS</a>.</p><p>The PigLatin language is SQL-like in nature but has many notable and significant differences. Similarly, while it is a pretty good workhorse (often times faster to write than writing the MR code by hand), its certainly not always the most pleasant language to use.</p><p>In general, Pig is used for <b><i>repeatable</i></b> (in the sense that such processing should <b>always</b> occur on data) processing of large datasets. That is, it is best used for running jobs that performs the same transformations on data each and every time new data exists. Though it can be used in an ad-hoc fashion, it is usually not a very pleasant experience.</p><h2>Hive</h2><p>Apache Hive is another data processing tool usually backed by HDFS. It provides a SQL-like language called HiveQL (which is far more similar to SQL than PigLatin) to interface with your distributed data. Similarly to Pig, it also supports Java-based UDFs.</p><p>Hive is typically geared toward performing analytic queries on big data (though it is not limited to this). Due to the nature of analytics, this sort of processing is not always necessary to repeat on all the data every time it&rsquo;s processed. Regardless, since many people are already familiar with SQL, writing a HiveQL query to represent complicated analysis is typically easier and more straight-forward than performing the same processing in Pig.</p><h2>Why not just pick one?</h2><p>Everyones use case is different and building maintainable systems is about picking the right tool. No one can deny the <a href="https://developer.yahoo.com/blogs/hadoop/comparing-pig-latin-sql-constructing-data-processing-pipelines-444.html" target="_blank">importance of Pig</a> in the modern world of big data processing. However, I maintain that doing research-like/ad-hoc analytics is often more intuitive and straight-forward using Hive.</p><p>In any case, this post isnt about defending one or the other but instead how to use them together. Now, lets move on!</p><h2>What kind of system are we building?</h2><p align="center"><img src="/images/hive-pig-analysis-arch.png" alt="image" width="250" height="200" data-orig-width="344" data-orig-height="288"/></p><p>Consider the diagram above. The particular kind of system I am envisioning while writing this is one such that Pig is used for the bulk of data processing and normalization while Hive is used&ndash; more or less&ndash; purely for analytics. This allows us to write the data processing code one time in Pig and move on to Hive for any ad-hoc insights were looking to derive.</p><p>I know that this looks trivial at a glance, but recall that each of those black boxes can be arbitrarily complex. That is, any processing/analytics data you want could be contained within those boxes. Similarly, such a system allows you to quickly determine the value of other insights which you believe may be valuable. If you find that the results are useful enough, you can always then incorporate them into your long-lived data processing directly. This allows you to be more <a href="http://www.agilemanifesto.org/" target="_blank">agile</a> without building extra components that may only possess marginal utility.</p><h2>How does this work (the concept)?</h2><p>As the diagram states, I am assuming that Pig is performing most of the heavy lifting in the system. In short, the final output stage from your Pig jobs will be the input to an <b>external</b> Hive table. By sharing a common HDFS path and with the use of a small script, you can automatically update Hive to be queryable from your processing results.</p><h2>How were going to implement this (pseudo-example)</h2><p>Now well be walking though a pseduo-example. That is, I will give the outline of what the code may look like here in a system, however, it wont be complete. Building an entire data warehouse is not something were interested in here! First, the PigLatin:</p>

```sql
data = LOAD '$INPUT_DATA' USING YourSpecialLoader;

-- Perform data processing
-- ...
-- Finally:
-- output_data = FOREACH GENERATE ...

STORE output_data INTO '$OUTPUT_DIRECTORY' USING OrcStorage();
```

<p>Of note here is that were using <a href="https://cwiki.apache.org/confluence/display/Hive/LanguageManual+ORC" target="_blank">OrcStorage</a>. Since this is our final Pig stage in our data processing, well want to output the data in a way thats most efficient to perform our analytics with Hive. According to the manual linked above, ORC provides a highly efficient way to store Hive data. It was designed to overcome limitations of the other Hive file formats. </p><p>Now the Hive table definition:</p>


```sql
CREATE EXTERNAL TABLE my_analytics(id BIGINT, name STRING)
PARTITIONED BY (date STRING)
STORED AS ORC
LOCATION '/hdfs/path/to/pig/output/dir/root';
```

<p>The first thing to notice is the location of the external table. Particularly, we will point to the top-level directory of where all of our pig output is going (i.e. each pig run should be in a sub-directory contained within this root).</p>
<p>You will also notice that this is an external table partitioned by date. An external Hive table is one where the data is managed by a third-party source rather than Hive itself (in this case, our Pig job). Partitions in Hive are, more or less, subdirectories within a location. They are in the format of key=value where key is the partitioning key and value is the corresponding value of appropriate type. This implies that the pig script running for some time 201508210800 would write its data to <i>/hdfs/path/to/pig/output/dir/root/date=201508210800</i>.</p><p>Finally, a bash script to run your Pig job and update Hive:</p>

```bash
#!/bin/bash
DATETIME=$1
DATABASE_NAME=YourDatabase
TABLE_NAME=YourTable

hive -e "using $DATABASE_NAME; ALTER TABLE $TABLE_NAME ADD PARTITION (date='$DATETIME');"
```

<p>The trick with this whole system is that you have to add a partition to the Hive table so that it picks up the new data. In this script we simply add a partition to Hive containing the relevant datetime. When this script is finished, the data will be loaded in Hive and can be queried immediately.</p><h2>Running this is a little cumbersome</h2><p>Well, yes. Doing anything manually is a bit of a pain. However, given these pieces, you can actually generate full data pipelines to do everything for you. That is, the final Pig script in conjunction with the bash script can be run via an <a href="http://oozie.apache.org/" target="_blank">Oozie</a> Workflow to ensure the data is process and the partition is added to Hive.</p><h2>Conclusion</h2><p>Data processing is a very real problem today. We are overwhelmed by the sheer volume of data around us and determining important characteristics of data is non-trivial. However, building a system conducive to finding these insights is crucial for performing this task effectively. Using Pig and Hive together may very well be the system youre looking for.</p>
