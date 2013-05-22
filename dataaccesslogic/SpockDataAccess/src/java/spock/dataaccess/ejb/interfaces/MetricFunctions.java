package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.Metric;
/**
 *
 * @author Loren Chorley
 */
@Remote
public interface MetricFunctions extends Serializable, BasicEntity<Metric, String> {
    
}
