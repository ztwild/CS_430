package hw3.yahtzee;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Component representing a Yahtzee flash cube.  Each cube will broadcast
 * a PingMessage to the left and right every POLL_INTERVAL ms.  If no reply
 * is received within TIMEOUT ms, the cube assumes it has no neighbor
 * in that direction.  If a reply is received, the PingReplyMessage contains
 * the sender's current display value.  This is ignored for a right ping,
 * but for a left ping this value is the number of cubes to the receiver's left.
 */
public class Cube extends ThreadedComponent
{
//  public static final int POLL_INTERVAL = 50; // ms
//  public static final int TIMEOUT = 250;
  
  public static final int POLL_INTERVAL = 500; // ms
  public static final int TIMEOUT = 2500;
  
  public static final boolean LEFT = true;
  public static final boolean RIGHT = false;
  public static final String[] START_ARR = {"S", "T", "A", "R", "T"};
  public static final String[] DONE_ARR = {"D", "O", "N", "E"};
  
  private Map<Integer, Boolean> pending;
  private Status status;
  private int index;
  private int inrow;
  private int value;
  private int total;
  private Random rand;
  
  private TimerComponent timer;
  
  public Cube(TimerComponent timer)
  {
    this.timer = timer;
    this.status = Status.START;
    this.index = 0;
    this.inrow = 0;
    this.pending = new HashMap<Integer, Boolean>();
    this.value = 0;
    this.total = 0;
    this.rand = new Random();
  }
  
  @Override
  public void send(IMessage message)
  {
    message.dispatch(this);
  }
  
  /** Logic for checking neighbor to left **/
//Requesting for index for the right to know where it is
  public void handlePingLeftRequest(PingLeftRequestMessage msg){
    int id = msg.getId();
    PingRightReplyMessage reply = new PingRightReplyMessage(id, this, index, total); 
    Universe.broadcastRight(reply);
  }
  
  /** Handles PingReplyMessages **/
  public void handlePingRightReply(PingRightReplyMessage msg){
    int id = msg.getCorrelationId();
    Boolean key = pending.remove(id);
    if(key != null){
      index = msg.getIndex() + 1;
      total += msg.getTotal();
      updateStatus();
    }
  }
  
  /** Logic for checking neighbor to Right **/
//Requesting for how many in row so it knows when all are in line
  public void handlePingRightRequest(PingRightRequestMessage msg){
    int id = msg.getId();
    PingLeftReplyMessage reply = new PingLeftReplyMessage(id, this, inrow);
    Universe.broadcastLeft(reply);
  }
  
  public void handlePingLeftReply(PingLeftReplyMessage msg){
    int id = msg.getCorrelationId();
    Boolean key = pending.remove(id);
    if(key != null){
      inrow = msg.getInline();
      updateStatus();
    }
  }
  
  /** Handles TimeoutMessage **/
  public void handleTimeout(TimeoutMessage msg){
    int id = msg.getCorrelationId();
    Boolean left = pending.remove(id);
    if (left != null){
      if(left)  { inrow = index; }
      else      { index = 0; }
      updateStatus();
    }
  }  
  
  public void updateStatus(){
    switch(status){
    case START: 
      if(inrow == 0){ 
        Universe.updateDisplay(this, "?"); 
      }
      else{
        Universe.updateDisplay(this, START_ARR[index]);
        if(inrow == 4) {
          status = Status.GENERATING;
          value = rand.nextInt(6) + 1;
        }
      }
      break;
    
//    case GENERATING:
//      
//      if(inrow == 0){
//        status = Status.SCORING;
//      }
//      else{
//        Universe.updateDisplay(this, START_ARR[index]);
//        value = rand.nextInt(6) + 1;
//      }
//      break;
    
//    case SCORING:
//      if(inrow == 0){ Universe.updateDisplay(this, " "+value); }
//      else{
//        if(inrow == 4) { status = Status.DONE; }
//        if(index < 4){
//          Universe.updateDisplay(this, DONE_ARR[index]);
//        }else{
//          Universe.updateDisplay(this, ""+23);
//        }
//      }
//      break;
    
    }
    
  }
  
  /** Handles PingRequestMessages **/
  
  
  
  
    

  @Override
  public void start()
  {
    Timer timer = new Timer(true);
    timer.scheduleAtFixedRate(new PingTimerTask(), 0, POLL_INTERVAL);
  }
  
  public class PingTimerTask extends TimerTask{

    @Override
    public void run() {
      
      PingLeftRequestMessage requestLeft = new PingLeftRequestMessage(Cube.this); //requesting index
      int idLeft = requestLeft.getId();
      pending.put(idLeft, RIGHT);
      Universe.broadcastLeft(requestLeft); 
      timer.send(new SetTimeoutMessage(Cube.this, idLeft, TIMEOUT));
      
      PingRightRequestMessage requestRight = new PingRightRequestMessage(Cube.this); //requesting blocks in row
      int idRight = requestRight.getId();
      pending.put(idRight, LEFT);
      Universe.broadcastRight(requestRight); 
      timer.send(new SetTimeoutMessage(Cube.this, idRight, TIMEOUT));
      
    }
  }
  
}
