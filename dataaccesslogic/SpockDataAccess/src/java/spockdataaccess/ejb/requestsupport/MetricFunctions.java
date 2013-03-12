package spockdataaccess.ejb.requestsupport;

import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import spockdataaccess.entity.Metric;

/**
 *
 * @author Loren Chorley
 */
public class MetricFunctions extends BasicEntity<Metric, String> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.MetricFunctions");
    
    public MetricFunctions(EntityManager em) {
        super(em);
    }
    
    @Override
    protected Metric newEntity(String id) {
        Metric x = new Metric();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(Metric sourceEntity, Metric targetEntity) {
        targetEntity.setCodeURL(sourceEntity.getCodeURL());
    }

    @Override
    protected String getEntityID(Metric entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "Metric";
    }

    @Override
    protected Class getEntityClass() {
        return Metric.class;
    }

    @Override
    protected void verifyBusinessLogic(Metric entity) {
        
    }
    
}
