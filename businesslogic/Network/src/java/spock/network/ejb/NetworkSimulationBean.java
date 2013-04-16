package spock.network.ejb;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.Singleton;
import javax.ejb.LocalBean;

/**
 *
 * @author Loren Chorley
 */
@Singleton
@LocalBean
public class NetworkSimulationBean {
    private static final Logger logger = Logger.getLogger("spock.network.ejb.NetworkSimulationBean");
    
    public void start() {
        logger.log(Level.INFO,
                   "Network simulation started!",
                   new Object[] { });
    }
    
}
