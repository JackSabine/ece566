#include <stdio.h>
#include <stdbool.h>

int reduce_or(int a);

int bit(int d, int bit)
{
  return (d >> bit)&0x1;
}

int any(int x)
{
  for(int i=0; i<32; i++)
    if (bit(x,i))
      return true;
  return false;
}

int main()
{

  for(int i=-999;i<20000; i++)
    {
      if (reduce_or(i) != any(i))
	{
	  printf("reduce_or(%d) produced %d but %d was expected.\n",i,reduce_or(i),any(i)); 
	  return 1;
	}
    }

  return 0;
}
