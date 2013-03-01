package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Environment;
import spockdataaccess.entity.Experiment;
import spockdataaccess.entity.Network;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.ExperimentFunctions");
    
    private EntityManager em;
    
    public ExperimentFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Creates a new experiment record in the database.
     * @param id the ID of the experiment
     */
    public void createExperiment(String id) {
        
        try {
        
            Experiment experiment = new Experiment();
            experiment.setId(id);
            experiment.setIsActive(Boolean.FALSE);

            em.persist(experiment);
            
            logger.log(Level.INFO,
                       "Created and persisted experiment: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.createExperiment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Sets the activation status of an experiment. True indicates that the experiment is currently active or running and may be stopped, and false that it is available to be started.
     * @param id the ID of the experiment
     * @param activation the new activation status of the experiment
     */
    public void setExperimentActivation(String id, boolean activation) {
        try {
        
            Experiment experiment = em.find(Experiment.class, id);
            
            if (experiment != null) {
                experiment.setIsActive(activation);
            } else {
                throw new EJBException("Experiment '" + id + "' not found when attempting to set activation property");
            }
        
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.setExperimentActivation threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Creates a new experiment record in the database.
     * @param id the ID of the experiment
     */
    public void removeExperiment(String id) {
        
        try {
        
            Experiment experiment = em.find(Experiment.class, id);
            em.remove(experiment);
            
            logger.log(Level.INFO,
                       "Removed experiment: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.removeExperiment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Adds a network to an experiment
     * @param experimentId the experiment to add the network to
     * @param networkId the network to add to the experiment
     */
    public void addNetworkToExperiment(String experimentId, String networkId) {
        
        try {
        
            Experiment experiment = em.find(Experiment.class, experimentId);
            Network network = em.find(Network.class, networkId);
            
            if (experiment == null) {
                throw new EJBException("addNetworkToExperiment threw: Experiment '" + experimentId + "' does not exist.");
            } else if (network == null) {
                throw new EJBException("addNetworkToExperiment threw: Network '" + networkId + "' does not exist.");
            }
            
            logger.log(Level.INFO,
                       "Attemping to added network {0} to experiment {1}",
                       new Object[] { network.getId(), experiment.getId() });
            
            experiment.addNetwork(network);
            
            logger.log(Level.INFO,
                       "Added network {0} to experiment {1}",
                       new Object[] { networkId, experimentId });
            
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.addNetworkToExperiment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Removes a network from an experiment
     * @param experimentId the experiment to add the network to
     * @param networkId the network to add to the experiment
     */
    public void removeNetworkFromExperiment(String experimentId, String networkId) {
        
        try {
        
            Experiment experiment = em.find(Experiment.class, experimentId);
            Network network = em.find(Network.class, networkId);
            
            if (experiment == null) {
                throw new EJBException("addNetworkToExperiment threw: Experiment '" + experimentId + "' does not exist.");
            } else if (network == null) {
                throw new EJBException("addNetworkToExperiment threw: Network '" + networkId + "' does not exist.");
            }
            
            logger.log(Level.INFO,
                       "Attemping to remove network {0} to experiment {1}",
                       new Object[] { network.getId(), experiment.getId() });
            
            experiment.removeNetwork(network);
            
            logger.log(Level.INFO,
                       "Removed network {0} to experiment {1}",
                       new Object[] { networkId, experimentId });
            
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.removeNetworkFromExperiment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Adds an environment to an experiment
     * @param experimentId the experiment to add the environment to
     * @param environmentId the environment to add to the experiment
     */
    public void addEnvironmentToExperiment(String experimentId, String environmentId) {
        
        try {
        
            Experiment experiment = em.find(Experiment.class, experimentId);
            Environment environment = em.find(Environment.class, environmentId);
            
            if (experiment == null) {
                throw new EJBException("addEnvironmentToExperiment threw: Experiment '" + experimentId + "' does not exist.");
            } else if (environment == null) {
                throw new EJBException("addEnvironmentToExperiment threw: Environment '" + environmentId + "' does not exist.");
            }
            
            logger.log(Level.INFO,
                       "Attemping to added environment {0} to experiment {1}",
                       new Object[] { environment.getId(), experiment.getId() });
            
            experiment.addEnvironment(environment);
            
            logger.log(Level.INFO,
                       "Added environment {0} to experiment {1}",
                       new Object[] { environmentId, experimentId });
            
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.addEnvironmentToExperiment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Removes an environment from an experiment
     * @param experimentId the experiment to add the environment to
     * @param environmentId the environment to add to the experiment
     */
    public void removeEnvironmentFromExperiment(String experimentId, String environmentId) {
        
        try {
        
            Experiment experiment = em.find(Experiment.class, experimentId);
            Environment environment = em.find(Environment.class, environmentId);
            
            if (experiment == null) {
                throw new EJBException("addEnvironmentToExperiment threw: Experiment '" + experimentId + "' does not exist.");
            } else if (environment == null) {
                throw new EJBException("addEnvironmentToExperiment threw: Environment '" + environmentId + "' does not exist.");
            }
            
            logger.log(Level.INFO,
                       "Attemping to remove environment {0} to experiment {1}",
                       new Object[] { environment.getId(), experiment.getId() });
            
            experiment.removeEnvironment(environment);
            
            logger.log(Level.INFO,
                       "Removed environment {0} to experiment {1}",
                       new Object[] { environmentId, experimentId });
            
        } catch (Exception ex) {
            throw new EJBException("ExperimentFunctions.removeEnvironmentFromExperiment threw: " + ex.getMessage());
        }
        
    }
    
}
