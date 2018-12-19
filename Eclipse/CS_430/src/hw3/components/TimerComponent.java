package hw3.components;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Callable;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Timer component.  Sending a SetTimeoutMessage to this component
 * will cause a TimeoutMessage to be sent to the caller after
 * the timeout value given in the message.  The TimeoutMessage
 * will contain the "original ID" from the SetTimeoutMessage
 * as its correlation id.
 */
public class TimerComponent extends Component
{
	
  protected Timer timer;
  
  protected TimerComponent()
  {
    timer = new Timer();
  }  

	/**
   * Sends a message to this component. The implementation
   * of this method could vary between components but in all
   * case it should accept messages without blocking.
   * @param message
   *   the message to send
   */	
  @Override
  public void send(IMessage message)
  {
    message.dispatch(this);
  }
  
  public void handleSetTimeout(SetTimeoutMessage message){
    TimerTask task = new TimerTask() {
      public void run() {
        TimeoutMessage tm = new TimeoutMessage(message.getOriginalId(), TimerComponent.this);
        message.getSender().send(tm);
      }
    };
    timer.schedule(task, message.getTimeout());
  }
  
  /**
   * Signals to this component that it may begin processing messages.
   */
  @Override
  public void start()
  {
    // TODO Auto-generated method stub
  }
  
}
