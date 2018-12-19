package test;

import java.util.Random;

public class test {
  public static void main(String[] args){
    TestDigitSum();
  }
  
  public static void TestZeroSum(){
    ChallengeOne ch = new ChallengeOne();
    int[] input = new int[] {-4, 0, -1, 3, 2};
    int total = ch.SumOfZero(input);
    System.out.print("The total is "+total);
  }
  
  public static void TestDigitSum(){
    ChallengeOne ch = new ChallengeOne();
    Random rand = new Random();
    int input = rand.nextInt(10000);
    System.out.println("Random Number: "+input);
    
    int total = ch.SumOfDigits(input);
    System.out.print("The total is "+total);
  }
}
