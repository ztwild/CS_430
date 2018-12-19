package hw2;

import java.util.concurrent.TimeUnit;

public class CountDownLatch extends Object
{
	private int count;
	private Object lock;
	
	public CountDownLatch(int c){
		this.count = c;
	}
	
	public synchronized void await() throws InterruptedException{
		while(count > 0)
			wait();
	}
	
	public synchronized void countDown(){
		count--;
		if(count == 0)
			notifyAll();
	}
	
}