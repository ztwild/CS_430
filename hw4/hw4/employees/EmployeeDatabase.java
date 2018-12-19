package hw4.employees;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.ToIntFunction;
import java.util.stream.Collectors;

public class EmployeeDatabase
{
  private final ArrayList<Employee> employees;

  /**
   * Constructor from file. The file should be in CSV format where the first
   * row is headings. The valid headings are "First", "Last", "Salary",
   * "Department", and "Position".
   */
  public EmployeeDatabase(String filename)
  {
    this.employees = parseDB(filename);
  }

  /**
   * Alternate constructor for testing.
   */
  public EmployeeDatabase(ArrayList<Employee> employees) 
  {
    this.employees = employees;
  }

  public static ArrayList<Employee> parseDB(String filename) 
  {
    try
    {
      File f = new File(filename);
      Scanner s = new Scanner(f);
      int[] map = parseHeadings(s.nextLine());
      ArrayList<Employee> employees = new ArrayList<>();
      while (s.hasNextLine()) 
      {
        employees.add(parseEmployee(s.nextLine(), map));
      }
      s.close();
      return employees;
    }
    catch (FileNotFoundException e) 
    {
      System.out.println("Could not find " + filename);
      return null;
    }
  }

  public static int[] parseHeadings(String line)
  {
    String[] headings = line.split(",");
    int[] map = new int[headings.length];
    for (int i = 0; i < headings.length; i++) 
    {
      if (headings[i].equals("First")) 
      {
        map[0] = i;
      }
      else if (headings[i].equals("Last")) 
      {
        map[1] = i;
      }
      else if (headings[i].equals("Salary")) 
      {
        map[2] = i;
      }
      else if (headings[i].equals("Department"))
      {
        map[3] = i;
      } 
      else
      {
        map[4] = i;
      }
    }
    return map;
  }

  public static Employee parseEmployee(String line, int[] map) 
  {
    String[] words = line.split(",");
    return new Employee(words[map[0]], words[map[1]],
        Integer.parseInt(words[map[2]]), words[map[3]], words[map[4]]);
  }
  
  /**
   * Returns the average salary for all employees in the given department.
   * @param department
   * @return
   */
  public double averageSalaryForDepartment(String department)
  {
    // TODO
    throw new UnsupportedOperationException("TODO");
  }

  /** Returns map of of average salary by department. Each department
   * should be mapped to one double which is the average salary of the people
   * in that department.
   */
  public Map<String, Double> averageSalariesByDepartment()
  {
    // TODO
    throw new UnsupportedOperationException("TODO");
  }

  /**
   * Returns a list of the names of employees in the "Production" department,
   * sorted by last name.  Each tuple should be in the form 
   * (first_name, last_name). 
   */
  public List<Tuple<String, String>> listOfEmployeesInProduction()
  {
    // TODO
    throw new UnsupportedOperationException("TODO");
  }

  /**
   * Returns a list of the highest paid employees, ordered from highest salary
   * to lowest, as a list of tuples containing a last name and salary.
   * The length of returned list is determined by the 'howMany' parameter.
   * @param howMany
   * @return
   */
  public List<Tuple<String, Integer>> getHighestPaidEmployees(int howMany)
  {
    // TODO
    throw new UnsupportedOperationException("TODO");    
  }
  
  /**
   * Returns a map in which each position is mapped to a
   * list of last names of employees in that position, 
   * sorted alphabetically.
   * @return
   */
  public Map<String, List<String>> listEmployeesByPosition()
  {
    // TODO
    throw new UnsupportedOperationException("TODO");     
  }
  
  /**
   * Alternate implementation of above.  Do not modify this method; rather, 
   * implement the PositionLister consumer class below.
   * @return
   */
  public Map<String, List<String>> listEmployeesByPositionAlt()
  {
    PositionLister combiner = employees.stream()
        .sorted((lhs, rhs) -> lhs.getLastName().compareTo(rhs.getLastName()))
        .collect(PositionLister::new, PositionLister::accept, PositionLister::combine);
    return combiner.getMap();
  }
  
  public static class PositionLister implements Consumer<Employee>
  {
    public Map<String, List<String>> getMap()
    {
      // TODO
      throw new UnsupportedOperationException("TODO");     
    }

    public void accept(Employee e)
    {
      // TODO
      throw new UnsupportedOperationException("TODO");     
    }

    public void combine(PositionLister other)
    {
      // TODO
      throw new UnsupportedOperationException("TODO"); 
    }
  }
  

  /**
   * Generic pair class.
   * @param <T1>
   * @param <T2>
   */
  public static class Tuple<T1, T2> 
  {
    final T1 a;
    final T2 b;
    
    public Tuple(T1 a, T2 b) 
    {
      this.a = a;
      this.b = b;
    }    
    public T1 getFirst() { return a; }
    public T2 getSecond() { return b; }   
    public String toString()
    {
      return "<" + a.toString() + ", " + b.toString() + ">";
    }
  }
  

}
