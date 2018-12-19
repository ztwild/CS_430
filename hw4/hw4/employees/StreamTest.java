package hw4.employees;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

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
  
  public static int[] evensToFront(int[] arr)
  {
    // TODO
    return null;
  }
}
