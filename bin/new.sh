#!/bin/bash

DATE=$( date +%Y-%m-%d )
TITLE="$1"
TAGS="$2"
SLUG=$( printf "${TITLE}" | tr "[:space:]" "\-" | tr -d -C [A-Za-z0-9_-] | tr [A-Z] [a-z] )
FILENAME="${DATE}-${SLUG}.md"
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/../_posts"
FILEPATH=${DIR}/$FILENAME


read -r -d '' CONTENT <<- EOM
---
title: $TITLE
date: $DATE
tags: $TAGS
layout: post
---

<!-- more -->

EOM


echo -e "${CONTENT}" > ${FILEPATH}
echo ${FILEPATH}
