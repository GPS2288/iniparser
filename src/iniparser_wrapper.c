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

char* iniparser_getstring_wrapper(unsigned long* reslen, dictionary* d, const char* key, char* def)
{
	char* result = iniparser_getstring(d, key, def);
	*reslen = strlen(result);
	return result;
}
