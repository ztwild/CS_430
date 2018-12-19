package hw4.employees;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.OptionalDouble;
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
    double average;
    average = employees.stream()
        .filter(e -> e.getDepartment().equals(department) )
        .mapToDouble(Employee::getSalary)
        .average()
        .getAsDouble();
    return average;
  }

  /** Returns map of of average salary by department. Each department
   * should be mapped to one double which is the average salary of the people
   * in that department.
   */
  public Map<String, Double> averageSalariesByDepartment()
  {
    Map<String, Double> list;
    list = employees.stream()
        .collect(Collectors.groupingBy(
             Employee::getDepartment, 
             Collectors.averagingDouble(Employee::getSalary)
            ));
    return list;
  }

  /**
   * Returns a list of the names of employees in the "Production" department,
   * sorted by last name.  Each tuple should be in the form 
   * (first_name, last_name). 
   */
  public List<Tuple<String, String>> listOfEmployeesInProduction()
  {
    List<Tuple<String, String>> list;
    list = employees.stream()
        .filter(e -> e.getDepartment().equals("Production"))
        .sorted((e1, e2) -> e1.getLastName().compareTo(e2.getLastName()))
        .map(e -> new Tuple(e.getFirstName(), e.getLastName()))
        .collect(Collectors.toList());
    return list;
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
    List<Tuple<String, Integer>> list;
    list = employees.stream()
        .sorted((e1, e2) -> ((Integer)e1.getSalary()).compareTo((Integer)e2.getSalary()) )
        .map(e -> new Tuple(e.getLastName(), e.getSalary()))
        .limit(howMany)
        .collect(Collectors.toList());
    return list;
  }
  
  /**
   * Returns a map in which each position is mapped to a
   * list of last names of employees in that position, 
   * sorted alphabetically.
   * @return
   */
  public Map<String, List<String>> listEmployeesByPosition()
  {
    Map<String, List<String>> list;
    list = employees.stream()
        .sorted((e1, e2) -> e1.getLastName().compareTo(e2.getLastName()))
        .collect(Collectors.groupingBy(Employee::getPosition, 
            Collectors.mapping(Employee::getLastName, Collectors.toList())));
    return list;
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
    private Map<String, List<String>> map;
    public int count;
    
    public PositionLister(){
      this.map = new HashMap<String, List<String>>();
      this.count = 0;
    }

    public Map<String, List<String>> getMap()
    {
      return map;
    }

    public void accept(Employee e)
    {
      
      String pos = e.getPosition();
      String name = e.getLastName();
      
      if(map.containsKey(pos)){
//        List<String> list = map.get(pos);
//        list.add(name);
//        map.replace(pos, list);
        map.get(pos).add(name);
        count++; 
        
      }
      else{
        List<String> list = new ArrayList<String>();
        list.add(name);
        map.put(pos, list);
        count++; 

      }
      
    }

    public void combine(PositionLister other)
    {
      Map<String, List<String>> otherMap = other.getMap();
      for(Entry<String, List<String>> entry : otherMap.entrySet()){
        String pos = entry.getKey();
        List<String> otherList = entry.getValue();
        if(map.containsKey(pos)){
          List<String> list = map.get(pos);
          list.addAll(otherList);
          map.replace(pos, list);
          count += otherList.size()*2;
        }
        else{
          map.put(pos, otherList);
          count += otherList.size()*2;
        }
        
      }
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
