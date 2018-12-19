package hw3.yahtzee;

public class PingLeftRequestMessage extends AbstractMessage {

  public PingLeftRequestMessage(Component sender) {
    super(sender);
  }
  

  @Override
  public void dispatch(Component receiver){
    receiver.handlePingLeftRequest(this);
  }
}