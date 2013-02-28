/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.ejb;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.EJB;
import javax.ejb.Singleton;
import javax.ejb.Startup;

/**
 *
 * @author Loren Chorley
 */
@Singleton
@Startup
public class ConfigBean {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.ConfigBean");
    
    @EJB
    private RequestBean requestbean;
    
    @PostConstruct
    public void createData() {
        logger.log(Level.INFO,
                   "Starting to create testing data",
                   new Object[] { });
        
        requestbean.createExperiment("Experiment1");
        requestbean.createExperiment("Experiment2");
        requestbean.setExperimentActivation("Experiment2", true);
        
        requestbean.createConfig("setting1", "value1");
        
        logger.log(Level.INFO,
                   "Requested value of setting1: {0}",
                   new Object[] { requestbean.getConfig("setting1") });
        
        logger.log(Level.INFO,
                   "Finished creating testing data",
                   new Object[] { });
    }
    
    @PreDestroy
    public void deleteData() {
        logger.log(Level.INFO,
                   "Cleaning database. (Warning: Should only be used for testing!)",
                   new Object[] { });
        
        requestbean.cleanDatabase();
    }
    
}
