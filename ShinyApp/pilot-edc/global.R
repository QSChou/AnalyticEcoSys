## connect Teradata
library(DBI)
library(rJava)
library(RJDBC)
.jinit()
for(l in list.files('/usr/lib/tdch/1.4/lib/')){ .jaddClassPath(paste("/usr/lib/tdch/1.4/lib/",l,sep=""))}
for(l in list.files('/usr/hdp/2.3.0.0-2557/hive/lib/')){ .jaddClassPath(paste("/usr/hdp/2.3.0.0-2557/hive/lib/",l,sep=""))}
for(l in list.files('/usr/hdp/2.3.0.0-2557/hadoop/')){ .jaddClassPath(paste("/usr/hdp/2.3.0.0-2557/hadoop/",l,sep=""))}
#.jclassLoader()$setDebug(0L)
drv_td <- JDBC("com.teradata.jdbc.TeraDriver")
drv_hive <- JDBC("org.apache.hive.jdbc.HiveDriver","/usr/hdp/2.3.0.0-2557/hive/lib/hive-jdbc-1.2.1.2.3.0.0-2557.jar",identifier.quote="`")
