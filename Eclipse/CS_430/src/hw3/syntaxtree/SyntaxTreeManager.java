package hw3.syntaxtree;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

/**
 * Singleton class for managing the abstract syntax trees obtained
 * from parsing source files.
 * 
 * In this version, a sync lock is held
 * on the SyntaxTreeManager for the duration of every
 * call to the parser.
 */
public class SyntaxTreeManager
{
  
  /**
   * The actual cache of syntax trees
   */
//  private final Map<String, SyntaxTreeHolder> m_cache = new HashMap<String, SyntaxTreeHolder>();
  private final Map<String, Future<SyntaxTreeHolder>> m_cache = new HashMap<>();
  
  /**
   * Return the syntax tree for the given source file, parsing
   * the file if necessary.  
   * 
   * @param filename
   * @return
   */
//  public synchronized SyntaxTree getSyntaxTree(String filename)
//  {
//  	SyntaxTreeHolder holder = m_cache.get(filename);
//    if (holder == null) //if its not cached already
//    {
//      Parser parser = new Parser(filename);
//      SyntaxTree ast = parser.parse();
//      holder = new SyntaxTreeHolder();
//      holder.set(ast);
//      m_cache.put(filename, holder);
//    }
//    return holder.get();
//  	
//  }
  
  public SyntaxTree getSyntaxTree(String filename)
  {
  	while(true){
  		Future<SyntaxTreeHolder> f = m_cache.get(filename);
  		if(f == null){
  			Callable<SyntaxTreeHolder> eval = new Callable<SyntaxTreeHolder>(){
  				public SyntaxTreeHolder call() throws InterruptedException{
  					Parser parser = new Parser(filename);
    	      SyntaxTree ast = parser.parse();
    	      SyntaxTreeHolder holder = new SyntaxTreeHolder();
    	      holder.set(ast);
    	      return holder;
  				}
  			};
  			FutureTask<SyntaxTreeHolder> ft = new FutureTask<SyntaxTreeHolder>(eval);
  			f = m_cache.putIfAbsent(filename, ft);
  			if(f == null){ f = ft; ft.run(); }
  		}
  		try															{ return f.get().get(); } 
  		catch (CancellationException e)	{ m_cache.remove(filename); } 
  		catch (ExecutionException e) 		{ e.printStackTrace(); }
  		catch (InterruptedException e) 	{ } 
  		
  	}
  	
  }

  /**
   * Helper class for storing completed syntax trees.  Doesn't
   * do much at this point, but could be useful...
   */
	private static class SyntaxTreeHolder
	{
	  private SyntaxTree m_ast;
		
	  
	  public SyntaxTree get()
	  {
	  	return m_ast;
	  }
	  
	  public void set(SyntaxTree ast)
	  {
	    m_ast = ast;
	  }
	}
}
