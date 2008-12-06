package fj.control.db;

import fj.F;
import fj.Function;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.concurrent.Callable;

/**
 * The DB monad represents a database action, or a value within the context of a database connection.
 */
public abstract class DB<A> {

  /**
   * Executes the database action, given a database connection.
   *
   * @param c The connection against which to execute the action.
   * @return The result of the action.
   * @throws SQLException if a database error occurred.
   */
  public abstract A run(final Connection c) throws SQLException;

  /**
   * Constructs a database action as a function from a database connection to a value.
   *
   * @param f A function from a database connection to a value.
   * @return A database action representing the given function.
   */
  public static <A> DB<A> db(final F<Connection, A> f) {
    return new DB<A>() {
      public A run(final Connection c) {
        return f.f(c);
      }
    };
  }

  /**
   * Returns the callable-valued function projection of this database action.
   *
   * @return The callable-valued function which is isomorphic to this database action.
   */
  public F<Connection, Callable<A>> asFunction() {
    return new F<Connection, Callable<A>>() {
      public Callable<A> f(final Connection c) {
        return new Callable<A>() {
          public A call() throws Exception {
            return run(c);
          }
        };
      }
    };
  }

  /**
   * Map a function over the result of this action.
   *
   * @param f The function to map over the result.
   * @return A new database action that applies the given function to the result of this action.
   */
  public <B> DB<B> map(final F<A, B> f) {
    return new DB<B>() {
      public B run(final Connection c) throws SQLException {
        return f.f(DB.this.run(c));
      }
    };
  }

  /**
   * Promotes any given function so that it transforms between values in the database.
   *
   * @param f The function to promote.
   * @return A function equivalent to the given one, which operates on values in the database.
   */
  public static <A, B> F<DB<A>, DB<B>> liftM(final F<A, B> f) {
    return new F<DB<A>, DB<B>>() {
      public DB<B> f(final DB<A> a) {
        return a.map(f);
      }
    };
  }

  /**
   * Constructs a database action that returns the given value completely intact.
   *
   * @param a A value to be wrapped in a database action.
   * @return A new database action that returns the given value.
   */
  public static <A> DB<A> unit(final A a) {
    return new DB<A>() {
      public A run(final Connection c) {
        return a;
      }
    };
  }

  /**
   * Binds the given action across the result of this database action.
   *
   * @param f The function to bind across the result of this database action.
   * @return A new database action equivalent to applying the given function to the result of this action.
   */
  public <B> DB<B> bind(final F<A, DB<B>> f) {
    return new DB<B>() {
      public B run(final Connection c) throws SQLException {
        return f.f(DB.this.run(c)).run(c);
      }
    };
  }

  /**
   * Removes one layer of monadic structure.
   *
   * @param a A database action that results in another.
   * @return A new database action equivalent to the result of the given action.
   */
  public static <A> DB<A> join(final DB<DB<A>> a) {
    return a.bind(Function.<DB<A>>identity());
  }
}
