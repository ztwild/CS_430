package hw3.yahtzee;

public class PingRequestMessage extends AbstractMessage{
  
  protected final boolean left;
  public PingRequestMessage(Component sender, boolean left) {
    super(sender);
    this.left= left;
  }

  public boolean fromLeft(){
    return left;
  }
  
  @Override
  public void dispatch(Component receiver){
    receiver.handlePingRequest(this);
  }
  
}
