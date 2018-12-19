package hw2;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Date;
import java.util.Scanner;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;


public class FileLogger
{
  private String filename;
  private BlockingQueue queue;
  
  public FileLogger(String filename)
  {
    this.filename = filename;
    this.queue = new ArrayBlockingQueue(10);
    Thread dequeueThread = new Dequeue();
    dequeueThread.start();
  }
  
  public void log(String msg)
  {
    Date d = new Date();
    try {
      queue.put(d + " " + msg);
    } 
    catch (InterruptedException e) { e.printStackTrace(); }
  }
  
  private class Dequeue extends Thread{
	  
	  public void run(){
			while(true){
				if(!queue.isEmpty()){
					// timestamp when log method was called with this message
					Date d = new Date();
					try
					{
					  String msg = (String)queue.take();
					  // argument 'true' means append to existing file
					  OutputStream os = new FileOutputStream(filename, true);
					  PrintWriter pw = new PrintWriter(os);
					  pw.println(d + " " + msg);
					  pw.close();
					}
					catch (FileNotFoundException e)
					{
					  System.err.println("Unable to open log file: " + filename); 
					} 
					catch (InterruptedException e) {
					  System.err.println("Unable to dequeue message");
					  e.printStackTrace();
					}
				}
			}
	  }
  }
 
}



