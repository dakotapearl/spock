package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.Metric;
/**
 *
 * @author Loren Chorley
 */
public interface MetricFunctions extends Serializable, BasicEntity<Metric, String> {
    
}
