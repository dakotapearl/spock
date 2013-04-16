package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
public interface ExperimentFunctions extends Serializable, BasicEntity<Experiment, String> {
    
    public BasicEntityCollection<Experiment, Network, String> Networks();
    public BasicEntityCollection<Experiment, Environment, String> Environments();
    public BasicEntityCollection<Experiment, UserInterface, Long> UserInterfaces();
    
}
