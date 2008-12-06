package fj.control.db;

import fj.Unit;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 * Performs database I/O, in order to read or write the database state.
 */
public final class DbState {
  private final Connector pc;
  private final DB<Unit> terminal;

  private DbState(final Connector pc, final DB<Unit> terminal) {
    this.pc = pc;
    this.terminal = terminal;
  }

  /**
   * A simple connector (the default) that gets connections to the given database URL from the driver manager.
   *
   * @param url The database URL to connect to.
   * @return A connector that generates connections to the given database.
   */
  public static Connector driverManager(final String url) {
    return new Connector() {
      public Connection connect() throws SQLException {
        return DriverManager.getConnection(url);
      }
    };
  }

  /**
   * Creates a database state reader given a connection URL.
   *
   * @param url The connection URL to the database.
   * @return A database state reader that reads the given database.
   */
  public static DbState reader(final String url) {
    return new DbState(driverManager(url), rollback);
  }

  /**
   * Creates a database state writer given a connection URL.
   *
   * @param url The connection URL to the database.
   * @return A database state writer that writes the given database.
   */
  public static DbState writer(final String url) {
    return new DbState(driverManager(url), commit);
  }

  /**
   * Returns a new reader that reads the database via the given Connector.
   *
   * @param pc A connector with which to generate database connections.
   * @return A new reader that reads the database via the given Connector.
   */
  public static DbState reader(final Connector pc) {
    return new DbState(pc, rollback);
  }

  /**
   * Returns a new writer that writes the database via the given Connector.
   *
   * @param pc A connector with which to generate database connections.
   * @return A new writer that writes the database via the given Connector.
   */
  public static DbState writer(final Connector pc) {
    return new DbState(pc, rollback);
  }

  private static final DB<Unit> rollback = new DB<Unit>() {
    public Unit run(final Connection c) throws SQLException {
      c.rollback();
      return Unit.unit();
    }
  };

  private static final DB<Unit> commit = new DB<Unit>() {
    public Unit run(final Connection c) throws SQLException {
      c.commit();
      return Unit.unit();
    }
  };

  /**
   * Runs the given database action as a single transaction.
   *
   * @param dba A database action to run.
   * @return The result of running the action against the database.
   * @throws SQLException in case of a database error.
   */
  public <A> A run(final DB<A> dba) throws SQLException {
    final Connection c = pc.connect();
    try {
      final A a = dba.run(c);
      terminal.run(c);
      return a;
    } catch (SQLException e) {
      c.rollback();
      throw e;
    }
    finally {
      c.close();
    }
  }
}
