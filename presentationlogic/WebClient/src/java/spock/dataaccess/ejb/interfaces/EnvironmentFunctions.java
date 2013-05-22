package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentNode;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Metric;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface EnvironmentFunctions extends Serializable, BasicEntity<Environment, String> {
    
    public static final boolean INPUT_INTERFACE = true;
    public static final boolean OUTPUT_INTERFACE = false;

    public BasicEntityCollection<Environment, Experiment, String> Experiments();
    public BasicEntityCollection<Environment, EnvironmentNode, Long> Nodes();
    public BasicEntityCollection<Environment, EnvironmentInterface, Long> Interfaces();
    public BasicEntityCollection<Environment, Metric, String> Metrics();
    
}
