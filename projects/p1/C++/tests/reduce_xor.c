#include <stdio.h>

int reduce_xor(int a);

int bit(int d, int bit)
{
  return (d >> bit)&0x1;
}

int parity(int x)
{
  int parity=0;
  for(int i=0; i<32; i++)
    if (bit(x,i))
      parity ^= 0x1;
  return parity;
}

int main()
{

  for(int i=-999;i<20000; i++)
    {
      if (reduce_xor(i) != parity(i))
	{
	  printf("reduce_xor(%d) produced %d but %d was expected.\n",i,reduce_xor(i),parity(i)); 
	  return 1;
	}
    }

  return 0;
}
