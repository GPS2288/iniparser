/* Wrapper for iniparser
 * This file contains helpers in C
 *
 * Copyright 2013 Georg Poppe

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#include <string.h>
#include <stdlib.h>
#include "iniparser.h"

struct ptr_package
{
	char** c;
	unsigned long* l;
};

char* iniparser_getstring_wrapper(dictionary* d, const char* key, char* def, unsigned long* result_len)
{
	char* result = NULL;

	result = iniparser_getstring(d, key, def);
	*result_len = strlen(result);
	return result;
}

struct ptr_package iniparser_getstring_array_wrapper(dictionary* d, const char* key, const char* delimiters, int* size)
{
	int n;
	char** result_strs = NULL;
	unsigned long* result_len = NULL;
	struct ptr_package result;

	result_strs = iniparser_getstring_array(d, key, delimiters, size);
	result_len = malloc(*size * sizeof(long));
	for(n=0; n<*size; n++)
	{
		result_len[n] = strlen(result_strs[n]);
	}
	result.c = result_strs;
	result.l = result_len;

	return result;
}
