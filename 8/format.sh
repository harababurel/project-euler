#!/bin/bash
tr -d '\n' | sed 's/\(.\)/\1,/g' | awk '{print "["$0"]"}'
