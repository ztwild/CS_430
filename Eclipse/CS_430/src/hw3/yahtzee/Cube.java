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
  public static final int POLL_INTERVAL = 50; // ms
  public static final int TIMEOUT = 250; 
  public static final boolean LEFT = true;
  public static final boolean RIGHT = false;
  public static final String[] START_ARR = {"S", "T", "A", "R", "T"};
  public static final String[] DONE_ARR = {"D", "O", "N", "E", };
      
  private Map<Integer, Integer> pendingLeft;
  private Map<Integer, Integer> pendingRight;
  private Status status;
  private int index;
  private int inrow;
  private int roll;
  private int total;
  private Random rand;
  
  private TimerComponent timer;
  
  public Cube(TimerComponent timer)
  {
    this.timer = timer;
    this.status = Status.START;
    this.index = 0;
    this.inrow = 0;
    this.pendingLeft = new HashMap<Integer, Integer>();
    this.pendingRight = new HashMap<Integer, Integer>();
    this.roll = 0;
    this.total = 0;
    this.rand = new Random();
  }
  
  @Override
  public void send(IMessage message){
    message.dispatch(this);
  }
  
  
  
  public void updatePing(Status s){
    switch(this.status){
    case START:
      if(s == Status.DONE){
        this.status = Status.DONE;
        doneDisplay();
      }
      else if(inrow > 0){
        Universe.updateDisplay(Cube.this, START_ARR[index]);
        if(inrow == 4){
          this.status = Status.GENERATING;
          roll = rand.nextInt(6) + 1;
        }
      }
      break;
    case GENERATING:
      Universe.updateDisplay(Cube.this, START_ARR[index]);
      break;
    case SCORING:
      if(s == Status.GENERATING){
        this.status = Status.GENERATING;
        Universe.updateDisplay(Cube.this, START_ARR[index]);
      }
      else if(s == Status.SCORING){
        doneDisplay();
      }
      else if(inrow == 4){
        this.status = Status.DONE;
        doneDisplay();
      }
      break;
    case DONE:
//      if(inrow == 4){
//        this.status = Status.START;
//      }
      doneDisplay();
      break;
    }
  }
  
  public void handlePingReply(PingReplyMessage msg){
    int id = msg.getCorrelationId();
    Integer keyRight = pendingLeft.remove(id);
    
    if(keyRight != null){
      index = msg.getIndex() + 1;
      total = roll + msg.getTotal();
      updatePing(msg.getStatus());
    }
    
    Integer keyLeft = pendingRight.remove(id);
    if(keyLeft != null){
      inrow = msg.getInrow();
      updatePing(msg.getStatus());
    }
  }
  
  
  public void handlePingRequest(PingRequestMessage msg){
    PingReplyMessage reply;
    int id = msg.getId();
    if(msg.fromLeft()){
      reply = new PingReplyMessage(id, this, RIGHT, index, inrow, total, status);
      Universe.broadcastLeft(reply);
    }
    else{ //From right requesting index
      reply = new PingReplyMessage(id, this, LEFT, index, inrow, total, status); 
      Universe.broadcastRight(reply);
    }
  }
  
  
  public void updateTimeout(){
    if(inrow == 0){
      switch(this.status){
      case START:
        Universe.updateDisplay(Cube.this, "?");
        break;
      case GENERATING:
        this.status = Status.SCORING;
        Universe.updateDisplay(Cube.this, Integer.toString(roll));
        break;
      case SCORING:
        Universe.updateDisplay(Cube.this, Integer.toString(roll));
        break;
      case DONE:
        this.status = Status.START;
        Universe.updateDisplay(Cube.this, "?");
        break;
      }
    }
  }
  
  public void handleTimeout(TimeoutMessage msg){
    int id = msg.getCorrelationId();
    Integer keyLeft = pendingLeft.remove(id);
    if (keyLeft != null){
      index = 0;
      total = roll;
      updateTimeout();
    }
    
    Integer keyRight = pendingRight.remove(id);
    if (keyRight != null){
      this.inrow = index;
      updateTimeout();
    }
  }

  @Override
  public void start(){
    Timer timer = new Timer(true);
    timer.scheduleAtFixedRate(new PingTimerTask(), 0, POLL_INTERVAL);
  }
  
  public class PingTimerTask extends TimerTask{

    @Override
    public void run() {
      PingRequestMessage requestLeft = new PingRequestMessage(Cube.this, RIGHT); //requesting index
      int id = requestLeft.getId();
      pendingLeft.put(id, 1);
      Universe.broadcastLeft(requestLeft); 
      timer.send(new SetTimeoutMessage(Cube.this, id, TIMEOUT));
      
      PingRequestMessage requestRight = new PingRequestMessage(Cube.this, LEFT); //requesting blocks in row
      id = requestRight.getId();
      pendingRight.put(id, 1);
      Universe.broadcastRight(requestRight); 
      timer.send(new SetTimeoutMessage(Cube.this, id, TIMEOUT));
      
    }
  }
  
  public void doneDisplay(){
    if(index < 4) { Universe.updateDisplay(Cube.this, DONE_ARR[index]); }
    else          { Universe.updateDisplay(Cube.this, Integer.toString(total)); }
  }
  
}
