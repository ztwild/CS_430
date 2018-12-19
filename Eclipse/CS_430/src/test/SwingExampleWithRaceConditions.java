package test;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;



/**
 * THIS VERSION HAS RACE CONDITIONS, since the helper thread updates
 * the UI components in its own context.
 */
public class SwingExampleWithRaceConditions extends JPanel
{
  // Swing components
  private JButton button;
  private JLabel label;
  
  /**
   * The number of times the button has been clicked.
   */
  private int count;

  private SlowCalculator calc = new SlowCalculator();
  
  /**
   * Constructor creates all components that will
   * be contained in this panel.
   */
  public SwingExampleWithRaceConditions()
  {
    
    // create a label with some initial text, and
    // add it to the panel
    label = new JLabel("Push this button! ");
    this.add(label);
    
    // create a button and add an 
    // ActionListener to its list of listeners
    button = new JButton("Push me");
    ActionListener myListener = new MyButtonListener();
    button.addActionListener(myListener);
    
    // add the button to the panel
    this.add(button);  

  }
  
  /**
   * Entry point. This method should normally do 
   * nothing except (possibly) parse command-line
   * arguments and invoke a helper method for creating
   * and starting up the UI.
   */
  public static void main(String[] args)
  {
    Runnable r = new Runnable()
    {
      public void run()
      {
        createAndShow();
      }
    };
    SwingUtilities.invokeLater(r);
    System.out.println("Main thread exiting");
  }
  
  /**
   * Static helper method creates the frame and
   * makes it visible.
   */
  private static void createAndShow()
  {

    // create the frame
    JFrame frame = new JFrame("First Swing Example");
    
    // create an instance of our JPanel subclass and 
    // add it to the frame
    frame.getContentPane().add(new SwingExampleWithRaceConditions());
    
    // give it a nonzero size
    frame.setSize(300, 100);
    
    // we want to shut down the application if the 
    // "close" button is pressed on the frame
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    // make the frame visible and start the UI machinery
    frame.setVisible(true);
  }
  
  private class MyButtonListener implements ActionListener
  {
    @Override
    public void actionPerformed(ActionEvent event)
    {       
      System.out.println("Button pushed, incrementing count");
      new Thread(new Helper()).start();
    }
  }

  private class Helper implements Runnable
  {
    public void run()
    {
      count = calc.increment(count);
      label.setText("Pushed " + count + " times: ");         
    } 
  }
}