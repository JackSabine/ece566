#include <stdio.h>
#include <stdbool.h>

int reduce_and(int a);

int bit(int d, int bit)
{
  return (d >> bit)&0x1;
}

int all(int x)
{
  for(int i=0; i<32; i++)
    if (!bit(x,i))
      return false;
  return true;
}

int main()
{

  for(int i=-999;i<20000; i++)
    {
      if (reduce_and(i) != all(i))
	{
	  printf("reduce_and(%d) produced %d but %d was expected.\n",i,reduce_and(i),all(i)); 
	  return 1;
	}
    }

  return 0;
}
