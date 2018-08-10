{ /* array test */
  char s[] = "abc";
  int a[] = {1, 2, 3, 4};
  int b[3] = {1};

  /* string literal is a char array, the end \0 character is inclued in the array */
  l_assert(E, sizeof(s) == 4);

  /* the size of normal array */
  l_assert(E, sizeof(a) == 4*sizeof(int));

  /* array initialize test */
  l_assert(E, b[0] == 1);
  l_assert(E, b[1] == 0);
  l_assert(E, b[2] == 0);
}

{ /* string test */
  char* s = 0;
  l_assert(E, strlen(s) == 0);
}

{ /** very large memory space allocation **/
}

{ /** compare performance between loop assign and memory copy **/
}

{ /** compare performance between memory copy and struct assign **/
}

{ /** the difference between lua code "require(string)" and C api "luaopen_string(L)" **/
}

{ /** lua table test, use empty key "" to store a vlue **/
}

