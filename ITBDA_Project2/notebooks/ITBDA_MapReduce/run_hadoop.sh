
#!/usr/bin/env bash

# QUESTION 1.4 - HADOOP MAPREDUCE CONFIGURATION

set -euo pipefail

# Set project folder
cd "$(dirname "$0")"

# Accept keyword dynamically
if [ "$#" -ne 2 ]; then
  echo "Usage: bash run_hadoop.sh <keyword> <output>"
  exit 1
fi

KEYWORD="$1"
OUTPUT="$2"

# Check Hadoop installation
: "${HADOOP_HOME:?HADOOP_HOME is not set}"

# Find Hadoop Streaming
STREAMING_JAR=$(find "$HADOOP_HOME/share/hadoop/tools/lib" \
  -name 'hadoop-streaming-*.jar' -print -quit)

if [ ! -f "$STREAMING_JAR" ]; then
  echo "Hadoop Streaming JAR not found"
  exit 1
fi

# Prepare the news articles
Rscript MapReduce.R prepare

# Hadoop output must not already exist
if [ -e "$OUTPUT" ]; then
  echo "Output directory already exists: $OUTPUT"
  exit 1
fi

# Hadoop MapReduce job configuration
hadoop jar "$STREAMING_JAR" \
  -D mapreduce.framework.name=local \
  -D fs.defaultFS=file:/// \
  -D mapreduce.job.name="Keyword Location Search" \
  -D mapreduce.job.reduces=1 \
  -files MapReduce.R \
  -input "$PWD/prepared" \
  -output "$PWD/$OUTPUT" \
  -mapper "Rscript MapReduce.R mapper" \
  -reducer "Rscript MapReduce.R reducer" \
  -cmdenv "SEARCH_KEYWORD=$KEYWORD"

# Display final output
cat "$OUTPUT"/part-*


