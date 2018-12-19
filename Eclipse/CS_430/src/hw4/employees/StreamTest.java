package hw4.employees;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class StreamTest
{
  public static void main(String[] args)
  {
    
    int[] test = {3, 4, 10, 3, 5, 1, 8, 6};
    System.out.println(Arrays.toString(test));
    int[] result = evensToFront(test);
    System.out.println("Result  : " + Arrays.toString(result));
    System.out.println("Expected: [4, 10, 8, 6, 3, 3, 5, 1]");
  }
  
  public static int compare(int i1, int i2){
//    System.out.println(i1+" and "+i2+" are both even/odd "+(i1 % 2 == i2 % 2));
    if(i1 == i2) return 0;
    else if(i1%2 == i2%2) return 0;
    else return i1%2 == 0 ? -1 : 1;
  }
  
  public static int[] evensToFront(int[] arr)
  {
    List<Integer> list;
    list = IntStream.of(arr)
        .boxed()
        .sorted((i1, i2) -> compare(i1, i2))
        .collect(Collectors.toList());
    
    for(int i = 0; i < arr.length; i++) arr[i] = list.get(i);
    return arr;
  }
}
