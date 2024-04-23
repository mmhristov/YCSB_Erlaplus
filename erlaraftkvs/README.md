# Raftkvs
A binding for Raft key-value store implementations in Erlang.
Work in progress!


## Prerequisites:
Add local jinterface jar file to dependencies:
```
cd ..
cd jinterface
mvn install:install-file -Dfile="OtpErlang.jar" -DgroupId="site.ycsb" -DartifactId="erlaraftkvs-binding" -Dversion="1.14" -Dpackaging="jar"
```

## Quickstart:

### 1. Build binding:
```
cd ..
mvn -pl site.ycsb:erlaraftkvs-binding -am clean package
```

### 2. Set up raft cluster and start erlang client

### 4. Run binding in shell:
```
cd ..
bin/ycsb shell erlaraftkvs
```

### 5. Use shell for "insert" and "read" requests