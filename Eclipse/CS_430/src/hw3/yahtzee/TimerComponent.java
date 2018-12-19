package hw3.yahtzee;

import java.util.Timer;
import java.util.TimerTask;

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

  @Override
  public void start()
  {
  }
}
